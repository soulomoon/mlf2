### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-317-parser-library-qualified-import-reference-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser-combinator
library with the next coherent canonical parser-parity surface: qualified
imports, import aliases with and without `exposing`, and qualified references
for values, types, constructors, classes, and methods. The round must compare
the shared `.mlfp` parser-library projection with the Haskell canonical parser
projection and keep fixture roots as thin source/evidence harnesses.

This is parser parity only. Do not claim resolver/checker behavior, import
visibility semantics, backend/native support, package-manager behavior, driver
work, platform work, proof work, full parser parity, or self-boot progress.
Existing resolver/checker tests in `test/ProgramSpec.hs` may be used as source
syntax references, but acceptance for this round is parser-owned
source-text parse/projection/diagnostic evidence.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the grouped qualified import/reference parser behavior before
coding, then grow the same shared source-text parser-library path across
positive and negative evidence.

This round builds on the approved shared parser library from rounds 310-316.
Keep implementation rooted in
`test/programs/compiler-parser-parity/parser-library/`. Extend the shared
lexer/token layer, parser combinators, parser grammar, AST/projection
rendering, source spans, and diagnostics. Do not create a fixture-owned
`ParserParityParser.mlfp`, a per-fixture token stream, an exact-source
recognizer, or a separate parser package per test fixture.

The first public behavior/focused failing test is:

`MLF.Program parser parity / shared parser-owned .mlfp parser extends source-text grammar to qualified imports and references`

The first RED assertion should run at least two new public parser-parity
fixtures through `run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
and compare each output with committed canonical projection files:

- `qualified-import-alias-references`: a `Main` module with
  `import Core as C exposing (Eq, Token(..), answer, eq);`, unqualified and
  qualified value references (`answer`, `C.answer`), qualified type and
  constructor references (`C.Token`), a qualified class constraint
  (`C.Eq C.Token => Bool`), and a qualified method call (`C.eq Token C.Token`).
- `qualified-import-alias-only`: a `Main` module with `import Core as C;`
  and only qualified access, for example `def main : C.Token = C.Token;`.

Add one negative diagnostic path in the same round, such as a malformed import
alias with `as` but no alias name:

`import Core as exposing (answer);`

Render it through the same `renderParserNegativeEvidenceFromSourceText` path
with a stable parser-owned label such as `expected-import-alias@...` or a
narrower existing parser-owned label if the implementation can reuse one
honestly. Do not copy Megaparsec prose verbatim.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should compare Haskell canonical projections and shared parser-library
   projections for both new fixtures before implementation starts.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/qualified-import-alias-references/`
   and `test/conformance/mlfp/parser-parity/qualified-import-alias-only/`.
   Extend the canonical projection renderer in `test/ProgramParserParitySpec.hs`
   only as needed to render import aliases, import alias spans, import
   exposing lists with multiple items, qualified source types, qualified class
   constraints, qualified constructor patterns, and qualified value/class/method
   references.
3. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should have only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
4. Extend the shared parser-library lexer/token layer for the selected source:
   `import`, `as`, `exposing`, `class`, `data`, `instance` only if needed by
   the selected fixture source, alias/module identifiers such as `Core` and
   `C`, qualified names separated by `.`, `Eq`, `Token`, `answer`, `eq`,
   `main`, punctuation for `:`, `.`, `=`, `=>`, `->`, `(..)`, commas,
   braces/parens, and semicolons.
5. Extend the shared parser-combinator grammar for reusable import alias
   parsing, optional `exposing` parsing after aliases, qualified upper/lower
   identifiers, qualified source types, qualified class constraints, qualified
   constructor patterns, and qualified value/method references. Compose through
   `Parser`, `parserBind` or equivalent sequencing, `parserChoice`, span
   capture, diagnostic labeling, and EOF/end-state checks.
6. Extend parser-library AST/projection rendering and span constants for the
   two new fixtures so the `.mlfp` parser projection matches the committed
   canonical projection. If a projection key is needed, make it a parser-owned
   syntax-family key reached only after real token/grammar parsing, not an
   exact fixture key or source-text match.
7. Add a public malformed import-alias diagnostic fixture in
   `test/ProgramParserParitySpec.hs` that calls the same
   `renderParserNegativeEvidenceFromSourceText` path. Use this to prove the
   shared parser library reports a parser-owned diagnostic for the selected
   malformed alias syntax.
8. Add guard coverage against returning to fixture-key success shortcuts. The
   focused matcher should fail if the shared parser contains direct fixture-key
   branches such as `parseQualifiedImportAliasModule`,
   `parseQualifiedAliasOnlyModule`, `completeModuleKey
   "qualified-import-alias-references"`, or `completeModuleKey
   "qualified-import-alias-only"`.
9. Update bounded progress docs that already track parser parity, such as
   `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`, without claiming full parser parity,
   resolver/checker behavior beyond existing compiler support, backend support,
   platform work, driver work, proof work, or self-boot completion.
10. Run the focused RED/GREEN check, the full parser-parity group, direct
    smokes for all carried and new parser-parity fixtures, static banned-shape
    audits, `git diff --check`, `cabal build all`, `cabal test`, and
    `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED and GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to qualified imports and references/"'`
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
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/qualified-import-alias-references --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/qualified-import-alias-only --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span|typeFamily.*Span|family.*Span|typeLevel.*Span|gadt.*Span|existential.*Span|constructorForall.*Span|qualified.*Span|alias.*Span|importAlias.*Span'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|TypeFamilyTokens|FamilyTokens|GadtTokens|ExistentialTokens|QualifiedImportTokens|AliasOnlyTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens|typeFamilyTokens|familyTokens|gadtTokens|existentialTokens|qualifiedImportTokens|aliasOnlyTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens|type-family tokens|family tokens|gadt tokens|existential tokens|qualified-import tokens|alias-only tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Rejected-shape audit for this round:
  `rg -n 'parseQualifiedImportAliasModule|parseQualifiedAliasOnlyModule|completeModuleKey "qualified-import-alias-references"|completeModuleKey "qualified-import-alias-only"|moduleKey "qualified-import-alias-references"|moduleKey "qualified-import-alias-only"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
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

- `orchestrator/rounds/round-317/selection-record.json`
- `orchestrator/rounds/round-317/round-plan-record.json`
