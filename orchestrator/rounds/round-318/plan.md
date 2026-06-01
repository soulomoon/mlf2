### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-318-parser-library-multi-module-export-import-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser-combinator
library so it parses complete `.mlfp` programs with more than one module and
projects module-level export/import surfaces across those modules. The round
must prove this through at least two new parser-parity fixtures routed through
the same shared parser entrypoint:

- `multi-module-abstract-export-import`: a `Core` module exporting an
  abstract `Nat` type plus values, and a `User` module importing `Nat` and
  values without constructor exposure.
- `multi-module-recursive-adt-export-import`: a `Core` module exporting a
  class, multiple `(..)` ADTs, and a value, and a `User` module importing them
  through `exposing` while using nested case/constructor syntax already carried
  by earlier parser-library rounds.

This is parser parity only. Do not claim resolver/checker behavior, import
visibility semantics, backend/native support, package-manager behavior, driver
work, platform work, proof work, full parser parity, or self-boot completion.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the grouped behavior before coding:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces`

The matcher should compare the Haskell canonical parser projection and the
shared `.mlfp` parser-library projection for both new fixtures. Extend the
Haskell projection harness so `renderLocatedProjection` handles `P.Program`
with multiple modules instead of failing on anything other than one module.
That harness change is test support only; the production parser behavior stays
owned by the current canonical parser and the `.mlfp` parser-library fixture
path.

Keep implementation rooted in
`test/programs/compiler-parser-parity/parser-library/`. Extend the shared
lexer/token layer, parser combinators, program/module grammar, AST/projection
rendering, source spans, and diagnostics. The parser should parse a complete
program as a sequence of modules through parser-owned state/combinators. It
must not select a result by fixture name, by exact source text, by static token
stream constructors, or by a one-off parser function for either new fixture.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should fail until both new fixtures are parsed by the shared parser-library
   path and match committed canonical projection files.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/multi-module-abstract-export-import/`
   and
   `test/conformance/mlfp/parser-parity/multi-module-recursive-adt-export-import/`.
   Use the current `test/programs/recursive-adt/abstract-module-use.mlfp` and
   `test/programs/recursive-adt/module-integrated.mlfp` as source references,
   reducing only if needed to keep the parser-syntax surface bounded.
3. Extend `test/ProgramParserParitySpec.hs` projection support so canonical
   projections render every parsed module in source order, preserving the
   existing line format for `module`, `export`, `import`, `import alias`,
   `import exposing`, `data`, `constructor`, and `def` rows. The expected
   files should make multi-module ordering and abstract-vs-`(..)` export/import
   spans visible.
4. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should contain only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
5. Extend the shared parser-library lexer/token layer only for tokens needed by
   the selected multi-module sources. Reuse existing token categories for
   module headers, `export`, `import`, `exposing`, type exports, value exports,
   `(..)`, `class`, `data`, `deriving`, `def`, `case`, patterns, lambda,
   type arrows, and punctuation.
6. Extend the shared parser-combinator grammar from single-module parsing to a
   complete-program parser that accepts one or more modules and reaches EOF
   only after the whole program is consumed. Module parsing should compose the
   existing import/declaration/expression/type parsers rather than branching on
   fixture-specific module names or token-stream shapes.
7. Extend parser-library AST/projection rendering so the shared `.mlfp` parser
   emits the same projection rows as the Haskell canonical projection for both
   multi-module fixtures. If an internal projection discriminator is needed,
   derive it after real program/module parsing; do not use
   `multi-module-abstract-export-import` or
   `multi-module-recursive-adt-export-import` as parser success keys.
8. Add one negative diagnostic path in the same focused matcher, such as an
   `import Core exposing (Nat zero);` source that is missing the comma between
   exposing items. Render it through the same
   `renderParserNegativeEvidenceFromSourceText` path with a stable parser-owned
   label such as `expected-import-exposing-separator@...`, or a narrower
   existing label if the implementation can reuse one honestly.
9. Add guard coverage against fixture-key/parser-shortcut regressions. The
   focused matcher should fail if the shared parser contains direct branches
   such as `parseMultiModuleAbstractExportImport`,
   `parseMultiModuleRecursiveAdtExportImport`,
   `completeModuleKey "multi-module-abstract-export-import"`,
   `completeModuleKey "multi-module-recursive-adt-export-import"`, or
   equivalent `moduleKey`/`programKey` shortcuts.
10. Update bounded progress docs that already track parser parity, such as
    `CHANGELOG.md`, `implementation_notes.md`,
    `docs/mlfp-self-boot-readiness.md`, and
    `test/conformance/mlfp/README.md`, without claiming checker, resolver,
    backend, platform, driver, proof, full parser parity, or self-boot
    completion.
11. Run the focused RED/GREEN check, the full parser-parity group, direct
    smokes for all carried and new parser-parity fixtures, static
    banned-shape audits, `git diff --check`, `cabal build all`, `cabal test`,
    and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED and GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces/"'`
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
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-abstract-export-import --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-recursive-adt-export-import --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|multiModule.*Span|abstract.*Span|recursive.*Span|moduleIntegrated.*Span|module.*ProgramKey|completeProgramKey'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|MultiModuleTokens|AbstractExportTokens|RecursiveAdtExportTokens|ModuleIntegratedTokens|LexerOk (multiModuleTokens|abstractExportTokens|recursiveAdtExportTokens|moduleIntegratedTokens)|multi-module tokens|abstract-export tokens|recursive-adt tokens|module-integrated tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Rejected-shape audit for this round:
  `rg -n 'parseMultiModuleAbstractExportImport|parseMultiModuleRecursiveAdtExportImport|completeModuleKey "multi-module-abstract-export-import"|completeModuleKey "multi-module-recursive-adt-export-import"|moduleKey "multi-module-abstract-export-import"|moduleKey "multi-module-recursive-adt-export-import"|programKey "multi-module-abstract-export-import"|programKey "multi-module-recursive-adt-export-import"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  should produce no matches.
- Shared parser architecture audit:
  `rg -n 'parseCompleteProgram|parseCompleteModule|parserStateAtEnd state|ParserAtEnd|ParserNotAtEnd|parserBind|parserChoice|captureSpan|diagnosticLabel' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`
  should show the complete-program path is parser-owned and combinator-based.
- Diff and full gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-318/selection-record.json`
- `orchestrator/rounds/round-318/round-plan-record.json`
