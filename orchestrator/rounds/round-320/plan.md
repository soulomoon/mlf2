### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-320-parser-library-first-class-polymorphism-source-type-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses the canonical first-class-polymorphism source-type surface through one thin parser-parity fixture:

- a definition whose type is `(∀ a. a -> a) -> Bool`;
- a lambda parameter annotated as `∀ a. a -> a`;
- a second exported definition with type `∀ a. a -> a`;
- a let/application expression body that uses the polymorphic value.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for the new fixture exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses first-class polymorphic source types`

Add a new conformance fixture and thin public parser-parity harness named `first-class-polymorphism-source-types`. The source may be copied from `test/programs/unified/first-class-polymorphism.mlfp` or reduced only enough to keep the selected parser-syntax surface bounded. The fixture root under `test/programs/compiler-parser-parity/first-class-polymorphism-source-types/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. Reuse existing token scanning for `∀`, `.`, `->`, parentheses, identifiers, `let`, `in`, boolean literals, and semicolons where possible. Extend grammar functions for source types and definition rows by composing parser-owned combinators such as `parserBind`, `parserChoice`, `captureSpan`, and diagnostic labels. Do not add a fixture-owned parser package, do not key success on `first-class-polymorphism-source-types`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `first-class-polymorphism-source-types`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should make the module span, three exports, `usePoly`, `id`, and `main` definition spans visible.
3. Add the thin public harness under `test/programs/compiler-parser-parity/first-class-polymorphism-source-types/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in the parser-parity batch in `test/ProgramParserParitySpec.hs`, so the generated public CLI driver exercises it alongside the carried 21 parser-parity fixtures.
5. Extend the shared parser library under `test/programs/compiler-parser-parity/parser-library/` to parse the selected first-class polymorphic source types through parser-owned state/combinators. The parser should derive definition type text, expression text, and source spans from consumed tokens.
6. Add one public malformed source-type negative path through the generated parser-parity batch, such as a missing dot in `∀ a a -> a`. Render it through `renderParserNegativeEvidenceFromSourceText` with a stable parser-owned diagnostic label such as `expected-forall-dot@...` or a narrower honest existing label.
7. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-320 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `first-class-polymorphism-source-types`, whole-source recognition, `parseFirstClassPolymorphism...` entrypoints, and pre-rendered `usePoly`/`id`/`main` rows for this fixture.
8. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, or self-boot completion.
9. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the full parser-parity group, direct smoke/diff checks for every parser-parity fixture including the new one, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses first-class polymorphic source types/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses first-class polymorphic source types/"'`
- Negative source-type diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed first-class polymorphic source-type diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct smoke/diff check for every parser-parity fixture:
  ```sh
  for fixture in test/programs/compiler-parser-parity/*; do
    name=$(basename "$fixture")
    if [ "$name" = parser-library ]; then
      continue
    fi
    expected="test/conformance/mlfp/parser-parity/$name/expected/parser-program.txt"
    actual=$(mktemp)
    timeout 900 cabal run mlf2 -- run-program "$fixture" --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
    diff -u "$expected" "$actual"
    rm -f "$actual"
  done
  ```
- New-fixture shortcut audit:
  `rg -n 'parseFirstClassPolymorphism|completeModuleKey "first-class-polymorphism-source-types"|moduleKey "first-class-polymorphism-source-types"|programKey "first-class-polymorphism-source-types"|FirstClassPolymorphismTokens|LexerOk firstClassPolymorphismTokens|first-class-polymorphism-source-types tokens|defRows sourceFile "usePoly"|def usePoly type=\\(∀a\\. a -> a\\) -> Bool|def id type=∀a\\. a -> a' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-320/selection-record.json`
- `orchestrator/rounds/round-320/round-plan-record.json`
