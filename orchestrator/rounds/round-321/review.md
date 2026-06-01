### Checks Run
- Command: `git fetch origin master`
  Result: pass; fetched `origin/master`.
- Command: `git rev-parse HEAD origin/master FETCH_HEAD && git merge-base HEAD origin/master && git merge-base --is-ancestor origin/master HEAD && git merge-base --is-ancestor HEAD origin/master`
  Result: pass; `HEAD`, `origin/master`, `FETCH_HEAD`, and merge-base all resolved to `4f1bb94064d8c691e4582b1caab86942761b4c21`; both ancestor checks returned `0`.
- Command: `git diff --cached --name-status && git diff --cached --stat`
  Result: pass; staged diff is the round-321 parser-library, new fixture/oracle, docs, tests, and round metadata surface.
- Command: `git diff --name-status && git diff --stat`
  Result: pass before validation; no unstaged implementation diff. Full tests produced a generated `runtime/mlfp_io/target/release/libmlfp_io.d` path rewrite, which was restored before artifacts were written.
- Command: `git diff --check`
  Result: pass.
- Command: `git diff --cached --check`
  Result: pass.
- Command: `rg -n '^(<<<<<<<|=======|>>>>>>>)' .`
  Result: pass; no conflict markers.
- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses higher-order partial applications/"'`
  Result: pass; `1 example, 0 failures`, finished in `170.8945` seconds.
- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed higher-order partial-application diagnostics through public run-program/"'`
  Result: pass; `1 example, 0 failures`, finished in `272.0846` seconds.
- Command: `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  Result: pass; `1 example, 0 failures`, finished in `0.4651` seconds.
- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: pass; `15 examples, 0 failures`, finished in `792.0902` seconds.
- Command: `for fixture in test/programs/compiler-parser-parity/*; do name=$(basename "$fixture"); if [ "$name" = parser-library ]; then continue; fi; expected="test/conformance/mlfp/parser-parity/$name/expected/parser-program.txt"; actual=$(mktemp); timeout 900 cabal run mlf2 -- run-program "$fixture" --search-path test/programs/compiler-parser-parity/parser-library > "$actual"; diff -u "$expected" "$actual"; rm -f "$actual"; done`
  Result: pass; checked `23` parser-parity fixture roots with no diffs.
- Command: `rg -n 'parseHigherOrderPartialApplication|completeModuleKey "higher-order-partial-application"|moduleKey "higher-order-partial-application"|programKey "higher-order-partial-application"|HigherOrderPartialApplicationTokens|LexerOk higherOrderPartialApplicationTokens|higher-order-partial-application tokens|defRows sourceFile "keepLeft"|defRows sourceFile "apply"|def keepLeft type=Int -> Int -> Int expr=λx λy x|def main type=Int expr=apply \(keepLeft 1\)' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  Result: pass; no shortcut matches.
- Command: `cabal build all`
  Result: pass.
- Command: `cabal test`
  Result: pass; `2662 examples, 0 failures`, finished in `1161.0119` seconds.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; final line: `[thesis-gate] PASS: thesis conformance anchors are green`.

### Plan Compliance
- Step 1, focused public positive matcher and TDD evidence: met. `test/ProgramParserParitySpec.hs` contains the requested matcher. `implementation-notes.md` records the positive RED before implementation: shared parser returned `Right "parser-error\n"` instead of the committed projection, then current GREEN passed.
- Step 2, committed conformance fixture: met. `test/conformance/mlfp/parser-parity/higher-order-partial-application/src/Main.mlfp` and `expected/parser-program.txt` are present and expose module/export/definition spans for `keepLeft`, `apply`, and `main`.
- Step 3, thin public harness: met. `test/programs/compiler-parser-parity/higher-order-partial-application/ParserParityFixture.mlfp` owns only `sourceFile` and `sourceText`; `Main.mlfp` calls `renderParserParityProjectionFromSourceText`.
- Step 4, positive fixture registration and batch inclusion: met. `parserParityPositiveCases` includes `positive:higher-order-partial-application`, and the full parser-parity group plus direct fixture loop exercised it.
- Step 5, shared parser-library grammar extension: met. The implementation extends `ParserParityParser.mlfp`, `ParserParityLexer.mlfp`, `ParserParityParserCombinator.mlfp`, and `ParserParityDiagnostic.mlfp`; parsing remains in the shared parser-owned combinator/state path with `parserBind`, `parserChoice`, current-token diagnostics, and source spans from consumed tokens.
- Step 6, malformed parenthesized-application diagnostic: met. The generated public batch includes `negative:higher-order-partial-application`, rendered through `renderParserNegativeEvidenceFromSourceText`, and the focused diagnostic matcher passed with `expected-expression-close-paren@...`.
- Step 7, shortcut/static guards: met. Hspec static guard passed, and the explicit new-fixture `rg` audit returned no matches. A broader implementation search found only generic parser helpers and the diagnostic renderer, not a new fixture-specific token stream, parser entrypoint, success key, or pre-rendered `keepLeft`/`apply`/`main` rows.
- Step 8, bounded docs: met. Added docs describe a bounded parser-parity slice and explicitly avoid full parser parity, resolver/checker/backend/platform/driver/proof, milestone completion, or self-boot progress claims.
- Step 9, verification: met. All requested focused, task-specific, diff, build, full test, and thesis gates passed.

### Decision
**APPROVED**

### Evidence
Lineage is fresh against `origin/master` at `4f1bb94064d8c691e4582b1caab86942761b4c21`. `selection-record.json` and `round-plan-record.json` match the active `roadmap_id`, `roadmap_revision`, `roadmap_dir`, `milestone-4`, `direction-4a-canonical-parser-parity`, and `item-321-parser-library-higher-order-partial-application-extension`.

The round remains parser-parity only. The fixture harness is source/evidence-only, the new behavior flows through the shared parser-owned parser library, and no checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, milestone completion, or self-boot scope is claimed.

Status-only closeout is valid: the active `roadmap-view.json` contains anchor `milestone-4-completion`, and the approved closeout only asks the controller to add one compact completion pointer for round-321 without changing milestone status or future coordination.
