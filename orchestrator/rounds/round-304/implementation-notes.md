### Changes Made

- Replaced the review-rejected whole-source `.mlfp` tokenizer placeholder with
  parser-owned discrete tokens in
  `test/programs/compiler-parser-parity/basic-module-def-bool/ParserParityToken.mlfp`.
  The token stream now carries separate span-bearing token fields for
  `module`, `Main`, `export`, `(`, `main`, `)`, `{`, `def`, `:`, `Bool`, `=`,
  `true`, `;`, and `}`.
- Changed tokenizer failure behavior so the selected fixture source is checked
  token by token and malformed source returns `LexerError` with the relevant
  source span. The retry evidence mutates `export` to `exxort` and observes the
  exported-keyword span.
- Changed
  `test/programs/compiler-parser-parity/basic-module-def-bool/ParserParityParser.mlfp`
  to consume the parser-owned `TokenStream` before returning the positive AST.
  A mismatched `true` token in the `=` slot now returns `ParserError` with
  `ExpectedEquals`, rather than falling through to the hard-coded positive
  projection.
- Added focused retry evidence to `test/ProgramParserParitySpec.hs`. The new
  spec builds a temporary parser-parity package under `dist-newstyle/`, imports
  `renderParserParityRetryEvidence`, and checks positive token span evidence,
  lexer-negative evidence, and parser-negative evidence.
- Preserved the existing positive projection for the canonical parser and the
  parser-owned `.mlfp` package:

  ```text
  module Main span=test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:1-4:1
  export value main span=test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:21-1:25
  def main type=Bool expr=true span=test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:2:3-3:1
  ```

### Tests

- RED retry evidence:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp tokenizer and parser reject discrete token mismatches/"'`
  failed before the retry implementation because `ParserParityParser` did not
  export `renderParserParityRetryEvidence`.
- Package smoke:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  passed and printed the three-line positive parser projection above.
- Focused retry matcher:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp tokenizer and parser reject discrete token mismatches/"'`
  passed with 1 example and 0 failures.
- Parser parity neighbor matcher:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed with 2 examples and 0 failures.
- Diff check:
  `git diff --check` passed.
- Full build gate:
  `cabal build all` passed.
- Full test gate:
  `cabal test` passed with 2614 examples and 0 failures.
- Thesis gate:
  `./scripts/thesis-conformance-gate.sh` passed with
  `PASS: thesis conformance anchors are green`.

### Notes

- The parser-owned package now demonstrates the review-required negative paths:
  lexer mismatch reports
  `unexpected-source@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19`,
  and parser mismatch reports
  `expected-equals@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:2:21-2:25`.
- The retry stays inside the parser-parity test package and test support. No
  production facade, Prelude, checker, backend, driver, platform, compiler
  package, or proof implementation files were widened for this fix.
- Validation rewrote
  `runtime/mlfp_io/target/release/libmlfp_io.d` with the round worktree path.
  That generated validation churn was restored and is not part of the
  implementation.
- `orchestrator/state.json` was already modified in the assigned worktree and
  was not edited by this retry.
- Blockers: none.
