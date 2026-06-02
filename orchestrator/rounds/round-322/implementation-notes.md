### Changes Made
- CHANGELOG.md: recorded the bounded parser-parity fixture extension in Unreleased without claiming full parser self-boot.
- docs/mlfp-self-boot-readiness.md: documented the Round 322 parser-owned fixture coverage for typed local lets and annotated-lambda RHS expressions as a bounded slice.
- implementation_notes.md: added the Round 322 implementation note with the parser-owned local-function-flow scope.
- test/ProgramParserParitySpec.hs: added the higher-order-local-function-flow positive matcher, malformed typed-local-let diagnostic matcher, fixture registration, and fixture-specific shortcut/static guards.
- test/conformance/mlfp/README.md: listed the new bounded higher-order-local-function-flow parser-parity fixture.
- test/conformance/mlfp/parser-parity/higher-order-local-function-flow/src/Main.mlfp: added the new source fixture with a typed local let chain, annotated lambda RHS, captured local value, and higher-order call.
- test/conformance/mlfp/parser-parity/higher-order-local-function-flow/expected/parser-program.txt: added the expected parser-owned projection for the new fixture.
- test/programs/compiler-parser-parity/higher-order-local-function-flow/Main.mlfp: added the thin public harness for direct run-program projection output.
- test/programs/compiler-parser-parity/higher-order-local-function-flow/ParserParityFixture.mlfp: added the fixture source path and source text used by the thin harness.
- test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp: extended the parser-owned lexer with a digit-run scanner so local-let literals beyond the prior small fixed set are tokenized by the shared lexer path.
- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp: extended the parser-owned parser-combinator library for typed local let chains, annotated-lambda RHS expressions, and two-definition module bodies while keeping fixture output rendered through parsed rows rather than fixture shortcuts.

### Tests
- test/ProgramParserParitySpec.hs: focused RED before parser support, command `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "shared parser-owned .mlfp parser parses higher-order local function flow" --fail-on=empty'`, failed with `Right "parser-error\n"` instead of the expected projection after the expected fixture span was corrected.
- test/ProgramParserParitySpec.hs: focused GREEN for higher-order local function flow, same command, passed with `1 example, 0 failures` in 176.0010s.
- test/ProgramParserParitySpec.hs: malformed diagnostic matcher, command `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "parser-owned .mlfp parser reports malformed higher-order local function diagnostics through public run-program" --fail-on=empty'`, passed with `1 example, 0 failures` in 292.5729s.
- test/ProgramParserParitySpec.hs: parser-library shortcut/static guard, command `timeout 300 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints" --fail-on=empty'`, passed with `1 example, 0 failures` in 0.4955s after narrowing a raw `defRows sourceFile "main"` audit phrase that matched existing non-fixture parser-library code.
- Full parser-parity group: command `timeout 3600 cabal test mlf2-test --test-options='--ignore-dot-hspec --match "MLF.Program parser parity" --fail-on=empty'`, passed with `17 examples, 0 failures` in 1001.1606s.
- Direct smoke/diff check for every parser-parity fixture: looped over `test/programs/compiler-parser-parity/*`, skipped `parser-library`, ran `timeout 900 cabal run mlf2 -- run-program "$fixture" --search-path test/programs/compiler-parser-parity/parser-library`, and diffed against each expected `parser-program.txt`; exit 0 with no diffs.
- New-fixture shortcut audit: `rg -n 'parseHigherOrderLocalFunctionFlow|completeModuleKey "higher-order-local-function-flow"|moduleKey "higher-order-local-function-flow"|programKey "higher-order-local-function-flow"|HigherOrderLocalFunctionFlowTokens|LexerOk higherOrderLocalFunctionFlowTokens|higher-order-local-function-flow tokens|defRows sourceFile "use"|def main type=Int expr=let captured : Int = 41 in let f : Int -> Int = λ\(x : Int\) captured in use f|higher-order-local-function-flow parser negative expected-let-in@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`, exit 1 with no matches.
- git diff --check: passed before adding this notes file with exit 0 and no output.
- cabal build all: passed with exit 0.
- cabal test: passed with `2664 examples, 0 failures` in 1369.8708s.
- ./scripts/thesis-conformance-gate.sh: passed with exit 0; final line was `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
- No verification gate was skipped.
- The requested raw audit phrase `defRows sourceFile "main"` was not kept as a static guard because it matched existing generic parser-library code unrelated to the new fixture. The implemented audit keeps fixture-specific shortcuts out of the parser library and test harness instead.
- `runtime/mlfp_io/target/release/libmlfp_io.d` was temporarily rewritten by build tooling to the worktree absolute path and was restored manually; it is not part of the implementation diff.
- Preexisting controller-owned round artifacts and `orchestrator/state.json` were left untouched.
