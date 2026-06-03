### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-325-parser-library-higher-order-function-field-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-005`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded higher-order function-field source surface through a thin parser-parity fixture:

- a `FnBox(..)` export and `data FnBox = FnBox : (Int -> Int) -> FnBox;`;
- a `main` definition with nested typed local lets, where `f : Int -> Int` is an annotated lambda over a captured value;
- a `case FnBox f of { FnBox g -> g 0 }` expression whose constructor-pattern branch applies the extracted function.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `higher-order-function-field` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/higher-order-function-field/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses higher-order function fields`

Add a new conformance fixture and thin public parser-parity harness named `higher-order-function-field`. The source should be copied from `test/programs/unified/higher-order-function-field.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/higher-order-function-field/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. Extend grammar functions by composing parser-owned parser-state combinators; do not add a fixture-owned parser package, do not key success on `higher-order-function-field`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

Because this is checker-facing public evidence, preserve the rev-005 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. Do not add or require a repeated loop of separate `run-program` invocations for every parser-parity fixture. A single standalone smoke/diff for only the new fixture is allowed as public-interface package-root evidence; if used, it must be labelled as that reason rather than as the broad regression strategy.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `higher-order-function-field`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/higher-order-function-field/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the module span, `FnBox(..)` export span, `main` export span, `data FnBox` span, `FnBox` constructor span with type `(Int -> Int) -> FnBox`, and the `main` definition span with the nested-let/case expression rendered by the canonical parser.
3. Add the thin public harness under `test/programs/compiler-parser-parity/higher-order-function-field/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch. After registration, the batch should include the carried 25 positive parser-parity fixtures plus `positive:higher-order-function-field`.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so the shared declaration grammar can parse a data constructor field whose source type is parenthesized function type followed by an outer arrow. Reuse existing source-type combinators rather than hardcoding `FnBox` rows.
6. Extend shared source-expression parsing so `case` is an ordinary expression form usable as a definition body or let body. This slice only needs the bounded constructor application scrutinee and one constructor-pattern branch shape from the fixture, but it should derive expression text and spans from consumed tokens and reusable case-pattern/body helpers rather than adding a fixture-specific complete-module parser.
7. Add one public malformed function-field negative path through the generated parser-parity batch, such as omitting the case branch arrow in `case FnBox f of { FnBox g g 0 }`. Render it through `renderParserNegativeEvidenceFromSourceText` with the existing stable parser-owned `expected-case-branch-arrow@...` diagnostic if that is the honest failure category.
8. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-325 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `higher-order-function-field`, whole-source recognition, `parseHigherOrderFunctionField...` entrypoints, pre-rendered `FnBox`/`main` rows for this fixture, and static negative evidence strings for the new negative path.
9. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
10. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, one optional public-interface standalone smoke/diff for the new fixture only, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order function fields/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order function fields/"'`
- Negative function-field diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed higher-order function-field diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diff, justified only as package-root evidence for the new fixture:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/higher-order-function-field --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/higher-order-function-field/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseHigherOrderFunctionField|completeModuleKey "higher-order-function-field"|moduleKey "higher-order-function-field"|programKey "higher-order-function-field"|HigherOrderFunctionFieldTokens|LexerOk higherOrderFunctionFieldTokens|higher-order-function-field tokens|defRows sourceFile "FnBox"|defRows sourceFile "main"|constructor FnBox type=\\(Int -> Int\\) -> FnBox|def main type=Int expr=let captured : Int = 41 in let f : Int -> Int =|higher-order-function-field parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-325/selection-record.json`
- `orchestrator/rounds/round-325/round-plan-record.json`
