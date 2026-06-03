### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-327-parser-library-cross-module-let-polymorphism-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-005`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded authoritative cross-module let-polymorphism source surface through a thin parser-parity fixture:

- `module Core export (applyId) { ... }`;
- one `Core` definition, `def applyId : Int = let id = λx x in id 1;`;
- `module User export (main) { ... }`;
- `import Core exposing (applyId);`;
- one `User` definition, `def main : Int = applyId;`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `authoritative-cross-module-let-polymorphism` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism`

Add a new conformance fixture and thin public parser-parity harness named `authoritative-cross-module-let-polymorphism`. The source should be copied from `test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. The expected parser work is to make definition-led module bodies accept exactly one source definition, then let the existing multi-module program parser compose the `Core` and `User` modules through the shared module, export, import, source-definition, let/lambda/application, and span-rendering helpers. Do not add a fixture-owned parser package, do not key success on `authoritative-cross-module-let-polymorphism`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

Because this is parser-parity public evidence that runs through a checker-facing `.mlfp` program harness, preserve the rev-005 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. Do not add or require a repeated loop of separate `run-program` invocations for every parser-parity fixture. A single standalone smoke/diff for only the new fixture is allowed as public-interface package-root evidence; if used, it must be labelled as that reason rather than as the broad regression strategy.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `authoritative-cross-module-let-polymorphism`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the `Core` module span, `applyId` export span, `applyId` definition span with `Int` and `let id = λx x in id 1`, the `User` module span, `main` export span, `import Core exposing (applyId)` import rows, and the `main` definition span with `applyId`, all rendered by the canonical parser with token-derived spans.
3. Add the thin public harness under `test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch. After registration, the batch should include the carried 27 positive parser-parity fixtures plus `positive:authoritative-cross-module-let-polymorphism`.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so `parseDefinitionLedBodyRows` can parse a one-definition module body by composing `parseSourceDefinitionRows` with `finishExactModuleBodyRows`. Keep the existing two-definition and three-definition paths available for carried fixtures.
6. Ensure the two-module source parses through `parseCompleteMultiModuleProgram` and `parseSharedProgramModule` without fixture-specific branching. The `Core` module should finish after one parsed source definition, and the `User` module should continue to reuse parser-owned import/export and source-definition helpers before finalizing its module body.
7. Ensure source-definition semicolon diagnostics for the selected syntax remain parser-owned. If the current generic definition semicolon path only produces an unlabeled token failure, extend that shared path with the existing parser-owned def-semicolon diagnostic helper or an equivalent labelled diagnostic; do not add a fixture-specific negative renderer.
8. Add one public malformed cross-module-let negative path through the generated parser-parity batch, such as omitting the semicolon after `def applyId : Int = let id = λx x in id 1` in the `Core` module. Render it through `renderParserNegativeEvidenceFromSourceText` with the stable parser-owned `expected-def-semicolon@...` diagnostic if that is the honest failure category; if the implementer finds a more precise shared parser-owned category, use that category and keep the committed evidence public and labelled.
9. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-327 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `authoritative-cross-module-let-polymorphism`, whole-source recognition, `parseAuthoritativeCrossModuleLetPolymorphism...` entrypoints, pre-rendered `applyId`/`main` rows for this fixture, and static negative evidence strings for the new negative path.
10. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
11. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, one optional public-interface standalone smoke/diff for the new fixture only, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism/"'`
- Negative cross-module-let diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed authoritative cross-module let-polymorphism diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diff, justified only as package-root evidence for the new fixture:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseAuthoritativeCrossModuleLetPolymorphism|completeModuleKey "authoritative-cross-module-let-polymorphism"|moduleKey "authoritative-cross-module-let-polymorphism"|programKey "authoritative-cross-module-let-polymorphism"|AuthoritativeCrossModuleLetPolymorphismTokens|LexerOk authoritativeCrossModuleLetPolymorphismTokens|authoritative-cross-module-let-polymorphism tokens|defRows sourceFile "applyId"|defRows sourceFile "main"|def applyId type=Int expr=let id = λx x in id 1|authoritative-cross-module-let-polymorphism parser negative expected-def-semicolon@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-327/selection-record.json`
- `orchestrator/rounds/round-327/round-plan-record.json`
