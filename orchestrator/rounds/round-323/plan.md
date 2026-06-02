### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-323-parser-library-higher-order-returned-function-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded higher-order returned-function source surface through a thin parser-parity fixture:

- a `make : Int -> (Int -> Int)` definition whose annotated lambda body contains a typed local let binding;
- a typed local let body that returns another annotated lambda, `λ(x : Int) captured`;
- a `main` definition that applies a parenthesized function-valued result in callee position, `(make 41) 0`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `higher-order-returned-function` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/higher-order-returned-function/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses higher-order returned functions`

Add a new conformance fixture and thin public parser-parity harness named `higher-order-returned-function`. The source should be copied from `test/programs/unified/higher-order-returned-function.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/higher-order-returned-function/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. Reuse existing tokens for `let`, `in`, `λ`, identifiers, `:`, `->`, parentheses, and integer literals. Extend source-expression parsing by composing parser-owned combinators so annotated-lambda bodies and typed local-let bodies can return another annotated lambda, while parenthesized function-valued expressions remain ordinary expression atoms that may be applied. Do not add a fixture-owned parser package, do not key success on `higher-order-returned-function`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `higher-order-returned-function`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/higher-order-returned-function/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should make the module span, `main` export span, `make` definition span, and `main` definition span visible, with rendered rows for `def make type=Int -> (Int -> Int) expr=λ(base : Int) let captured : Int = base in λ(x : Int) captured` and `def main type=Int expr=(make 41) 0`.
3. Add the thin public harness under `test/programs/compiler-parser-parity/higher-order-returned-function/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch so the public CLI driver exercises it alongside the carried 24 parser-parity fixtures.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so the shared source-expression grammar parses annotated lambdas as valid returned values from typed local lets and annotated-lambda bodies through parser-owned state/combinators. Preserve parser-owned state threading and derive expression text and source spans from consumed tokens.
6. Add one public malformed returned-function negative path through the generated parser-parity batch, such as omitting the close parenthesis in the callee-position expression `def main : Int = (make 41 0;`. Render it through `renderParserNegativeEvidenceFromSourceText` with the existing stable parser-owned `expected-expression-close-paren@...` diagnostic if that is the honest failure category.
7. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-323 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `higher-order-returned-function`, whole-source recognition, `parseHigherOrderReturnedFunction...` entrypoints, pre-rendered `make`/`main` rows for this fixture, and static negative evidence strings for the new negative path.
8. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
9. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the full parser-parity group, direct smoke/diff checks for every parser-parity fixture including the new one, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order returned functions/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order returned functions/"'`
- Negative returned-function diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed higher-order returned-function diagnostics through public run-program/"'`
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
  `rg -n 'parseHigherOrderReturnedFunction|completeModuleKey "higher-order-returned-function"|moduleKey "higher-order-returned-function"|programKey "higher-order-returned-function"|HigherOrderReturnedFunctionTokens|LexerOk higherOrderReturnedFunctionTokens|higher-order-returned-function tokens|defRows sourceFile "make"|defRows sourceFile "main"|def make type=Int -> \(Int -> Int\) expr=λ\(base : Int\) let captured : Int = base in λ\(x : Int\) captured|def main type=Int expr=\(make 41\) 0|higher-order-returned-function parser negative expected-expression-close-paren@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-323/selection-record.json`
- `orchestrator/rounds/round-323/round-plan-record.json`
