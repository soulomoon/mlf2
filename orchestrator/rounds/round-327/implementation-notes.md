### Changes Made

- Added the bounded `authoritative-cross-module-let-polymorphism` parser-parity fixture under `test/conformance/mlfp/parser-parity/` with canonical source text and expected parser projection.
- Added the package-root harness under `test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism/` so the fixture runs through the shared parser-library entrypoint.
- Extended `test/ProgramParserParitySpec.hs` with the focused positive matcher, malformed cross-module-let negative matcher, aggregate batch registration, static shortcut guards, and explicit shortcut audit evidence for the new slice.
- Extended the shared parser-owned library, not a fixture-owned parser:
  - `ParserParityParser.mlfp` now parses a single-definition module body, a current-token definition semicolon diagnostic path, and the imported `def main : Int = applyId;` shape used by this bounded cross-module fixture.
  - `ParserParityParserCombinator.mlfp` now exposes `parserFailExpectedDefSemicolonAtCurrent` for precise malformed-definition diagnostics.
- Updated `CHANGELOG.md`, `implementation_notes.md`, `test/conformance/mlfp/README.md`, and `docs/mlfp-self-boot-readiness.md` to record bounded parser-parity coverage without claiming checker, resolver, backend, package-manager, full-parser-parity, or self-boot support.

### Tests

- Focused RED before implementation:
  - Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism/"'`
  - Result: failed as expected; `Finished in 176.2002 seconds`, `1 examples, 1 failures`; the shared parser returned `Right "parser-error\n"` before the parser-library extension.
- Focused GREEN after implementation:
  - Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism/"'`
  - Result: passed; `Finished in 180.7728 seconds`, `1 examples, 0 failures`.
- New malformed cross-module-let diagnostic matcher:
  - Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed authoritative cross-module let-polymorphism diagnostics through public run-program/"'`
  - Result: passed; `Finished in 315.6125 seconds`, `1 examples, 0 failures`.
- Parser-library shortcut/static guard:
  - Command: `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  - Result: passed; `Finished in 0.7058 seconds`, `1 examples, 0 failures`.
- Shared-context aggregate parser-parity batch:
  - Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
  - Result: passed; `Finished in 343.1172 seconds`, `1 examples, 0 failures`.
- Full parser-parity group:
  - Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  - Result: passed; `Finished in 1938.1955 seconds`, `25 examples, 0 failures`.
- Optional standalone new-fixture smoke/diff:
  - Command: `actual=$(mktemp); timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism --search-path test/programs/compiler-parser-parity/parser-library > "$actual"; diff -u test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/expected/parser-program.txt "$actual"; rm -f "$actual"`
  - Result: passed; exit code 0 and no diff output.
- New-fixture shortcut audit:
  - Command: `rg -n 'parseAuthoritativeCrossModuleLetPolymorphism|completeModuleKey "authoritative-cross-module-let-polymorphism"|moduleKey "authoritative-cross-module-let-polymorphism"|programKey "authoritative-cross-module-let-polymorphism"|AuthoritativeCrossModuleLetPolymorphismTokens|LexerOk authoritativeCrossModuleLetPolymorphismTokens|authoritative-cross-module-let-polymorphism tokens|defRows sourceFile "applyId"|defRows sourceFile "main"|def applyId type=Int expr=let id = λx x in id 1|authoritative-cross-module-let-polymorphism parser negative expected-def-semicolon@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  - Result: passed; no matches, `rg` exit code 1 as expected.
- Diff whitespace gate:
  - Command: `git diff --check`
  - Result: passed; no output.
- Build gate:
  - Command: `cabal build all`
  - Result: passed; exit code 0.
- Full test gate:
  - Command: `cabal test`
  - Result: passed; `Finished in 2167.8284 seconds`, `2672 examples, 0 failures`, `Test suite mlf2-test: PASS`.
- Thesis conformance gate:
  - Command: `./scripts/thesis-conformance-gate.sh`
  - Result: passed; final line was `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes

- Scope stayed parser-parity/library only. No checker, resolver, backend, platform, driver, proof, package-manager, full-parser-parity, or self-boot behavior was added.
- Broad parser-parity validation used the generated aggregate public CLI driver with labelled per-case sections, preserving rev-005 shared-context run discipline. The standalone smoke/diff was only package-root evidence for the new fixture.
- `runtime/mlfp_io/target/release/libmlfp_io.d` was rewritten by validation and restored with `git restore runtime/mlfp_io/target/release/libmlfp_io.d`; it is intentionally excluded from the round diff.
- No blockers remain.
