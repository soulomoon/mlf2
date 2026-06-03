### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-326-parser-library-authoritative-recursive-let-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-005`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded authoritative recursive-let source surface through a thin parser-parity fixture:

- `data Nat = Zero : Nat | Succ : Nat -> Nat;`;
- `main : Bool` with a typed local `peel : Nat -> Nat`;
- the local function RHS `λ(n : Nat) case n of { Zero -> Zero; Succ inner -> peel inner }`;
- an outer `case peel (Succ Zero) of { Zero -> true; Succ _ -> false }`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `authoritative-recursive-let` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/authoritative-recursive-let/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses authoritative recursive let flows`

Add a new conformance fixture and thin public parser-parity harness named `authoritative-recursive-let`. The source should be copied from `test/programs/unified/authoritative-recursive-let.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/authoritative-recursive-let/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. The expected parser work is to make typed-let RHS parsing accept an annotated lambda whose body composes through `parseSourceCaseExpression`, and to ensure the selected inner and outer case expressions render from consumed tokens. Do not add a fixture-owned parser package, do not key success on `authoritative-recursive-let`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

Because this is checker-facing public evidence, preserve the rev-005 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. Do not add or require a repeated loop of separate `run-program` invocations for every parser-parity fixture. A single standalone smoke/diff for only the new fixture is allowed as public-interface package-root evidence; if used, it must be labelled as that reason rather than as the broad regression strategy.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `authoritative-recursive-let`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/authoritative-recursive-let/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the module span, `Nat(..)` export span, `main` export span, `data Nat` span, `Zero`/`Succ` constructor spans, and the `main` definition span with the recursive typed-let and inner/outer case expressions rendered by the canonical parser.
3. Add the thin public harness under `test/programs/compiler-parser-parity/authoritative-recursive-let/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch. After registration, the batch should include the carried 26 positive parser-parity fixtures plus `positive:authoritative-recursive-let`.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so typed-let RHS parsing can parse an annotated lambda whose body is a `case` expression. Compose through parser-owned parser-state functions such as `parserBind`, `parserChoice`, `parseAnnotatedLambda...`, and `parseSourceCaseExpression`; do not introduce `parseAuthoritativeRecursiveLet...` or equivalent fixture-specific complete-module entrypoints.
6. If the current generic case-expression helper cannot render both selected branch lists from consumed tokens, extend that shared case branch parser for the bounded `Zero`/`Succ` and wildcard branch forms used here. Reuse existing case-pattern/body helpers where possible, and keep expression text and spans derived from tokens rather than static fixture rows.
7. Add one public malformed recursive-let negative path through the generated parser-parity batch, such as omitting the inner case branch arrow in `case n of { Zero Zero; Succ inner -> peel inner }`. Render it through `renderParserNegativeEvidenceFromSourceText` with the existing stable parser-owned `expected-case-branch-arrow@...` diagnostic if that is the honest failure category.
8. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-326 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `authoritative-recursive-let`, whole-source recognition, `parseAuthoritativeRecursiveLet...` entrypoints, pre-rendered `peel`/`main` rows for this fixture, and static negative evidence strings for the new negative path.
9. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
10. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, one optional public-interface standalone smoke/diff for the new fixture only, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative recursive let flows/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative recursive let flows/"'`
- Negative recursive-let diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed authoritative recursive-let diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diff, justified only as package-root evidence for the new fixture:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/authoritative-recursive-let --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/authoritative-recursive-let/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseAuthoritativeRecursiveLet|completeModuleKey "authoritative-recursive-let"|moduleKey "authoritative-recursive-let"|programKey "authoritative-recursive-let"|AuthoritativeRecursiveLetTokens|LexerOk authoritativeRecursiveLetTokens|authoritative-recursive-let tokens|defRows sourceFile "peel"|defRows sourceFile "main"|def main type=Bool expr=let peel : Nat -> Nat =|authoritative-recursive-let parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-326/selection-record.json`
- `orchestrator/rounds/round-326/round-plan-record.json`
