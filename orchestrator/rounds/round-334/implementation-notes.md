### Changes Made
- `test/conformance/mlfp/parser-parity/complex-recursive-program/src/Main.mlfp`:
  added the conformance fixture source copied from
  `test/programs/recursive-adt/complex-recursive-program.mlfp`.
- `test/conformance/mlfp/parser-parity/complex-recursive-program/expected/parser-program.txt`:
  added the canonical parser projection for the new bounded fixture.
- `test/programs/compiler-parser-parity/complex-recursive-program/Main.mlfp`
  and `test/programs/compiler-parser-parity/complex-recursive-program/ParserParityFixture.mlfp`:
  added the thin parser-owned package root that exposes `sourceFile` and
  `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- `test/ProgramParserParitySpec.hs`: registered the positive fixture in the
  aggregate batch, added direct shared-parser coverage, added malformed Tree
  traversal branch-arrow negative coverage, and extended shortcut/static guards
  for round-334 fixture, token, projection, exact-expression, and static
  negative shortcuts.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  extended the shared parser-library grammar for the selected
  `Eq`/`Nat deriving Eq`/`Tree` declaration sequence, eight-item export lists,
  four generic definition rows, and bounded nested parenthesized
  constructor/function applications in `main`.
- `test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp`:
  extended the bounded line-number successor table through the fixture's
  canonical final span.
- `implementation_notes.md`, `CHANGELOG.md`, and
  `docs/mlfp-self-boot-readiness.md`: recorded bounded parser-parity evidence
  and explicit non-claims only.

### Tests
- PASS `git diff --check`; exited 0 with no whitespace errors.
- PASS `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`; 45 examples, 0 failures, finished in 4603.6454 seconds.
- PASS focused iteration command `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity/shared parser-owned .mlfp parser parses complex recursive programs"'`; 1 example, 0 failures, finished in 246.3460 seconds.
- PASS `cabal run mlf2 -- check-program test/programs/compiler-parser-parity/complex-recursive-program --search-path test/programs/compiler-parser-parity/parser-library`; printed `OK`.

### Notes
- Scope stayed within the approved parser-parity slice. I did not edit any
  `state.json`, did not rewrite `plan.md`, did not merge, and did not add
  fixture-specific whole-source recognition, static negative shortcuts,
  pre-rendered rows, compatibility aliases, or canonical-parser bypasses.
- Iteration exposed and fixed three parser-library issues before final
  verification: a recursive top-level binding lookup in the new parenthesized
  application helpers, a runtime `expected a function` failure in the nested
  simple-second path, and line-number clamping after line 33 in the bounded
  parser-library lexer helper.
- `./scripts/thesis-conformance-gate.sh` was not run because the approved plan
  selected focused verification, this was not a milestone closeout, no thesis
  obligation ledgers were edited, and repo-facing notes make only bounded
  parser-parity evidence claims with explicit non-claims.
- Controller recommendation: advance round 334 to review.
