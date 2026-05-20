# Round 306 Implementation Notes

## Changes

- Added the canonical value-definition-list parser parity fixture:
  `test/conformance/mlfp/parser-parity/value-def-list-int-ref/`.
- Added the parser-owned public `.mlfp` package:
  `test/programs/compiler-parser-parity/value-def-list-int-ref/`.
- Extended `test/ProgramParserParitySpec.hs` so the Haskell canonical
  projection helper can render multiple value definitions, integer literal
  expressions, and lower-case value references.
- Added public `run-program` negative evidence for malformed value-definition
  sequencing through the parser-owned package.
- Updated readiness/progress docs only for this bounded parser-only tracer.

## RED / GREEN / Refactor

- RED positive:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans/"'`
  failed before implementation because the existing projection helper expected
  one value definition while the canonical parser produced two declarations.
- GREEN positive:
  the same focused matcher passed after adding the fixture, parser-owned
  package, and small projection helper expansion.
- RED negative:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed value-definition sequencing through public run-program/"'`
  failed before implementation because the parser-owned package did not export
  `renderValueDefListParserNegativeEvidence`.
- GREEN negative:
  the same focused matcher passed after adding the parser-owned sequencing
  diagnostic projection.
- Refactor:
  no broad parser surface was added. The only cleanup was keeping the Haskell
  projection rendering list-oriented and keeping all parser helpers inside the
  parser parity test package.

## Tests

- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans/"'`
  passed.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed value-definition sequencing through public run-program/"'`
  passed.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed with 6 examples and 0 failures.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref`
  passed and printed the committed projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  passed.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
  passed.
- `git diff --check` passed before closeout.
- `cabal build all && cabal test` passed with 2618 examples and 0 failures.
- `./scripts/thesis-conformance-gate.sh` passed.

## Docs Claim Audit

- `test/conformance/mlfp/README.md` now lists this fixture as value-definition
  list parser parity evidence.
- `docs/mlfp-self-boot-readiness.md` records only bounded parser/lexer seed
  progress and explicitly keeps checker, backend, driver, platform, compiler
  package, proof, and self-boot out of scope.
- `CHANGELOG.md` records the same bounded parser-only progress.
- No doc update claims full parser parity or parser-combinator completion.

## Generated Artifact Audit

- Running the gates rewrote
  `runtime/mlfp_io/target/release/libmlfp_io.d` with worktree-local absolute
  paths. That generated dependency-file churn was restored.
- `orchestrator/state.json` was already modified by the control plane to mark
  `round-306` active and was not edited by this implementation.

## Retry

- `git restore -- runtime/mlfp_io/target/release/libmlfp_io.d` restored the
  tracked generated depfile to the branch baseline without changing parser
  implementation files.
- `git status --porcelain=v1 -- runtime/mlfp_io/target` produced no output
  after the restore.
- `git diff --check` passed after the depfile restore and this retry note.

## Blockers

- None.
