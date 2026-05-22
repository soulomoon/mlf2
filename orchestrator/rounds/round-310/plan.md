### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-310-parser-library-consolidation`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Consolidate the carried round-304 through round-309 parser-parity fixtures onto
one shared parser-owned `.mlfp` parser library. The shared library must expose a
common complete-module parser entrypoint, use explicit parser-monad sequencing
or equivalent monadic parser state functions, scan source text through a normal
lexer/token path, and preserve the committed parser projection and diagnostic
evidence for every carried fixture.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that proves at least `basic-module-def-bool` and `import-exposing-def-bool`
execute through the same shared parser library root via `runProgramArgs
[fixtureRoot, "--search-path", sharedParserLibraryRoot]`. Then grow the shared
library until all carried positive and negative parser-parity fixtures use that
same parser entrypoint.

The shared parser-owned package should live under
`test/programs/compiler-parser-parity/parser-library/` and contain the common
source/cursor, token, AST/projection, lexer, parser-combinator, diagnostic, and
complete-module parser modules. Fixture roots under
`test/programs/compiler-parser-parity/<fixture>/` should become thin harnesses:
they may provide fixture source text, source file identity, and evidence
selection, but they must not each own independent `ParserParityParser.mlfp`
grammar modules or prebuilt exact-source token streams.

The common parser entrypoint should accept complete source text plus source-file
identity and return one shared parser result type. Parser implementation must
compose grammar functions through named parser-state helpers such as
`parserPure`, `parserBind` or `andThen`, `parserMap`, failure propagation,
choice, span capture, and diagnostic labeling. If `.mlfp` has no typeclass
support for `Monad`, do not claim a typeclass instance; use explicit named
parser functions.

Remove exact-source token-stream recognition from the normal success path. The
lexer may use broad string/`Char` helpers such as `stringCharAtOption`,
`stringSlice`, classification helpers, and cursor advancement, but it must not
recognize one fixture by matching the complete fixture text and returning a
hardcoded token stream. Golden source files and expected projections remain
test oracles only.

Keep the round inside parser parity. Do not add checker, resolver, backend,
driver, platform, proof, generic Prelude parser/monad API, package manager,
formatter, docs generator, REPL, or self-boot proof scope.

### Steps
1. Add the focused RED Hspec matcher in `test/ProgramParserParitySpec.hs`:
   `MLF.Program parser parity / shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint`. The first assertion should run at least the basic Bool and import-exposing fixture roots with `--search-path test/programs/compiler-parser-parity/parser-library` and compare each output to its committed canonical projection.
2. Add the shared parser library root under
   `test/programs/compiler-parser-parity/parser-library/`. Define shared
   parser-owned source/cursor, token, AST/projection, diagnostic,
   parser-state/combinator, lexer, and complete-module parser modules. The
   exported entrypoint should be common to all fixtures, not named for a single
   fixture family.
3. Convert `basic-module-def-bool` and `import-exposing-def-bool` into thin
   harnesses that provide fixture source/evidence selection and call the shared
   parser entrypoint through the search-path library. Confirm the first focused
   matcher turns GREEN before broadening.
4. Expand the same shared entrypoint to the carried positive fixtures for
   `value-def-list-int-ref`, `let-lambda-application`,
   `typed-annotation-types`, and `data-declaration-constructor-spans`. Preserve
   every committed `test/conformance/mlfp/parser-parity/*/expected/parser-program.txt`
   projection.
5. Replace negative evidence package generation in `ProgramParserParitySpec` so
   temporary evidence roots provide only `Main.mlfp` plus thin fixture/evidence
   selection modules and use `--search-path` to the shared parser library. Do
   not copy fixture-specific `ParserParitySource.mlfp`, `ParserParityToken.mlfp`,
   `ParserParityAst.mlfp`, or `ParserParityParser.mlfp` support modules into
   each temporary package.
6. Remove or rewrite the old per-fixture support modules so there is no normal
   success path based on fixture-specific `TokenStream` constructors,
   `parserMissing...Tokens` values, or complete-source recognition. Keep only
   the shared parser-library modules plus thin fixture harness modules.
7. Update bounded progress docs that already track parser parity, such as
   `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`, without claiming milestone-4 completion
   or self-boot progress.
8. Run the focused parser-parity checks, direct package smokes for all carried
   fixtures, exact-source-recognition audits, diff hygiene, full Cabal gate,
   and thesis gate before review.

### Verification
- RED evidence:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
- Focused shared-entrypoint GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
- Carried parser-parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
- Audit per-fixture parser removal and exact-source success-path removal:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityParser.mlfp -print`
  should show only the shared parser-library parser module, and review should
  reject any remaining complete-fixture-source recognizer that returns a
  prebuilt success token stream.
- Diff and full gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-310/selection-record.json`
- `orchestrator/rounds/round-310/round-plan-record.json`
