### Changes Made

- Copied the exact source files from the existing run-program conformance
  package fixtures into parser-parity package-layout fixtures for
  `package-cross-module-let` and `package-search-path-import`.
- Added expected parser-program projections that preserve individual source
  paths, module names, export rows, import rows, and explicit source ordering.
- Added thin parser-owned package roots that expose selected source-file
  path/text pairs and call the shared package-layout renderer.
- Extended `ParserParityParser.mlfp` with
  `renderParserParityPackageProjectionFromSourceTexts`, which parses each
  source through the existing source-file parser path before joining rows.
- Extended `ProgramParserParitySpec` with direct package parser assertions,
  aggregate positive package batch registration, one dynamic package-layout
  negative assertion, and shortcut/static guard phrases.
- Updated bounded repo-facing notes in `CHANGELOG.md`,
  `implementation_notes.md`, and `docs/mlfp-self-boot-readiness.md` without
  claiming full parser parity, package checker/resolver/backend,
  compiler-package, platform, driver, proof, or self-boot progress.

### Tests

- Passed:
  `git diff --check`
- Passed:
  `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  - Result: 60 examples, 0 failures.
- Passed:
  `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program shared conformance corpus"'`
  - Result: 5 examples, 0 failures.

### Notes

- No `state.json` files were edited.
- No merge was attempted.
- Thesis conformance gate was not run because the notes make only bounded
  parser-parity claims and explicitly leave thesis/self-boot package progress
  open.
- Controller should advance round-337 to review.
