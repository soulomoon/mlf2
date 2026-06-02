### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-322-parser-library-higher-order-local-function-flow-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded higher-order local-function-flow source surface through a thin parser-parity fixture:

- a top-level `use : (Int -> Int) -> Int` definition with an annotated function-valued lambda parameter;
- a `main` definition with a typed local value binding `captured : Int`;
- a second typed local binding `f : Int -> Int` whose RHS is an annotated lambda that closes over `captured`;
- a final expression `use f`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `higher-order-local-function-flow` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/higher-order-local-function-flow/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses higher-order local function flow`

Add a new conformance fixture and thin public parser-parity harness named `higher-order-local-function-flow`. The source should be copied from `test/programs/unified/higher-order-local-function-flow.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/higher-order-local-function-flow/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. Reuse existing tokens for `let`, `in`, `λ`, identifiers, `:`, `->`, parentheses, and integer literals. Extend source-expression parsing by composing parser-owned combinators so local `let` supports an optional source type annotation, RHS expressions can be annotated lambdas, and let bodies can recurse into another let before returning to application/atom parsing. Do not add a fixture-owned parser package, do not key success on `higher-order-local-function-flow`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `higher-order-local-function-flow`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/higher-order-local-function-flow/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should make the module span, `main` export span, `use` definition span, and `main` definition span visible.
3. Add the thin public harness under `test/programs/compiler-parser-parity/higher-order-local-function-flow/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch so the public CLI driver exercises it alongside the carried 23 parser-parity fixtures.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so the shared source-expression grammar parses typed local let chains and annotated-lambda RHS expressions through parser-owned state/combinators. Preserve parser-owned state threading and derive expression text and source spans from consumed tokens.
6. Add one public malformed typed-local-let negative path through the generated parser-parity batch, such as omitting the `in` after the local function binding. Render it through `renderParserNegativeEvidenceFromSourceText` with the existing stable parser-owned `expected-let-in@...` diagnostic if that is the honest failure category.
7. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-322 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `higher-order-local-function-flow`, whole-source recognition, `parseHigherOrderLocalFunctionFlow...` entrypoints, and pre-rendered `use`/`main` rows for this fixture.
8. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
9. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the full parser-parity group, direct smoke/diff checks for every parser-parity fixture including the new one, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order local function flow/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order local function flow/"'`
- Negative typed-local-let diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed higher-order local function diagnostics through public run-program/"'`
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
  `rg -n 'parseHigherOrderLocalFunctionFlow|completeModuleKey "higher-order-local-function-flow"|moduleKey "higher-order-local-function-flow"|programKey "higher-order-local-function-flow"|HigherOrderLocalFunctionFlowTokens|LexerOk higherOrderLocalFunctionFlowTokens|higher-order-local-function-flow tokens|defRows sourceFile "use"|defRows sourceFile "main"|def main type=Int expr=let captured : Int = 41 in let f : Int -> Int = λ\(x : Int\) captured in use f|higher-order-local-function-flow parser negative expected-let-in@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-322/selection-record.json`
- `orchestrator/rounds/round-322/round-plan-record.json`
