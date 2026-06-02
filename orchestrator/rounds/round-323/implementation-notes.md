### Changes Made
- `test/conformance/mlfp/parser-parity/higher-order-returned-function/src/Main.mlfp`: Added the higher-order returned-function parser-parity source fixture with an annotated lambda returning another annotated lambda through a typed local let, plus a parenthesized function-valued call site.
- `test/conformance/mlfp/parser-parity/higher-order-returned-function/expected/parser-program.txt`: Added the canonical parser projection expected output for the new fixture.
- `test/programs/compiler-parser-parity/higher-order-returned-function/Main.mlfp` and `test/programs/compiler-parser-parity/higher-order-returned-function/ParserParityFixture.mlfp`: Added the thin public harness that delegates to the shared parser-owned parser library.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: Extended the parser-combinator grammar for parenthesized arrow codomains, nested annotated-lambda bodies, typed local-let bodies that return annotated lambdas, and parenthesized function-valued callee atoms.
- `test/ProgramParserParitySpec.hs`: Added the focused positive matcher, malformed returned-function diagnostic matcher, generated batch registration, negative evidence, and widened static shortcut guards for round-323 fixture-specific keys/tokens/projection rows.
- `test/conformance/mlfp/README.md`, `docs/mlfp-self-boot-readiness.md`, `implementation_notes.md`, and `CHANGELOG.md`: Documented the bounded parser-parity extension without claiming full parser parity or self-boot readiness.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: Restored the tracked parent-checkout absolute path after build/test commands rewrote it to the round worktree path.

### Tests
- Focused RED, projection-shape adjustment: `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "shared parser-owned .mlfp parser parses higher-order returned functions" --fail-on=empty'` failed with `1 example, 1 failure` in 176.2763 seconds because the initial expected projection preserved parentheses that the canonical renderer removes.
- Focused RED, parser gap: the same matcher failed with `1 example, 1 failure` in 176.1618 seconds, producing `Right "parser-error\n"` for the new fixture before the parser-library extension.
- Focused GREEN: the same matcher passed with `1 example, 0 failures` in 177.9971 seconds.
- Malformed returned-function diagnostic matcher: `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "parser-owned .mlfp parser reports malformed higher-order returned-function diagnostics through public run-program" --fail-on=empty'` passed with `1 example, 0 failures` in 300.1703 seconds.
- Parser-library shortcut/static guard: `timeout 300 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints" --fail-on=empty'` passed with `1 example, 0 failures` in 0.5734 seconds, then passed again after widening the round-323 row-shape checks with `1 example, 0 failures` in 0.5781 seconds.
- Full parser-parity group: `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "MLF.Program parser parity" --fail-on=empty'` passed with `19 examples, 0 failures` in 1227.8006 seconds.
- Direct smoke/diff check for every parser-parity fixture: the first loop attempt failed because the first `cabal run mlf2` emitted one-time executable build/link stdout into the redirected output; rerunning the same fixture loop after the executable existed passed every fixture diff, including `higher-order-returned-function`.
- New-fixture shortcut audit: the raw plan phrase `defRows sourceFile "main"` matched the existing generic renderer; a refined fixture-specific audit excluding that broad generic phrase found no round-323 fixture parser/token/projection shortcut.
- `git diff --check`: passed before implementation notes were written and passed again after implementation notes were added.
- `cabal build all`: passed.
- `cabal test`: passed with `2666 examples, 0 failures` in 1645.2573 seconds.
- `./scripts/thesis-conformance-gate.sh`: passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
- No required verification gate was skipped.
- Controller-owned artifacts were present before implementation and were not edited by me: `orchestrator/state.json`, `orchestrator/rounds/round-323/plan.md`, `orchestrator/rounds/round-323/round-plan-record.json`, and `orchestrator/rounds/round-323/selection-record.json`.
- The broad `defRows sourceFile "main"` string is intentionally allowed because it belongs to the generic projection renderer, not to a fixture-specific shortcut.
