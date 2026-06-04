### Changes Made

- `test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp`:
  added the canonical source fixture copied from the recursive ADT/typeclass
  integration program.
- `test/conformance/mlfp/parser-parity/typeclass-integration/expected/parser-program.txt`:
  added the committed canonical parser-program projection for the fixture.
- `test/programs/compiler-parser-parity/typeclass-integration/Main.mlfp` and
  `test/programs/compiler-parser-parity/typeclass-integration/ParserParityFixture.mlfp`:
  added the thin parser-parity package root that exposes `sourceFile` and
  `sourceText` before calling the shared parser-library entrypoint.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  extended the shared parser-owned source-text path with dynamic support for
  the selected `Eq`/`Nat`/`Eq Nat` instance shape, nested case branch bodies,
  explicit instance method projection rows, and the selected top-level
  definitions without adding fixture-level source recognizers or pre-rendered
  projection rows.
- `test/ProgramParserParitySpec.hs`: registered the positive fixture in direct
  and aggregate parser-parity coverage, added one malformed nested-case
  negative evidence case, and extended shortcut/static guard audits for this
  round's forbidden fixture shortcuts.
- `implementation_notes.md`, `CHANGELOG.md`, and
  `docs/mlfp-self-boot-readiness.md`: recorded bounded parser-parity evidence
  and explicit non-claims only.

### Tests

- PASS: `ghcup run --ghc 9.14.1 -- cabal test -j1 mlf2-test --test-options='--match "recursive ADT typeclass integration"'`
  - Result: 3 examples, 0 failures.
  - Purpose: focused iteration over the new direct fixture, aggregate positive
    route, and negative evidence case.
- PASS: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  - Result: 36 examples, 0 failures.
  - Purpose: required aggregate parser-parity group, generated public CLI
    driver, negative evidence paths, and shortcut/static guards.
- PASS: `git diff --check`
  - Result: command exited 0 with no reported whitespace errors.

### Notes

- `./scripts/thesis-conformance-gate.sh` was not run because this round did not
  edit thesis obligation ledgers and makes only bounded parser-parity notes, not
  a thesis/readiness claim beyond the selected fixture evidence.
- The first non-serialized focused attempt hit Cabal/GHC object rename races
  before tests executed. The focused iteration was rerun with `-j1`; the final
  required parser-parity command was run exactly as requested and passed.
- No `selection-record.json`, `round-plan-record.json`, `review-record.json`,
  `closeout-record.json`, or paired JSON round records were created.
