### Changes Made
- `test/conformance/mlfp/parser-parity/deriving-eq/`: Added the exact
  `test/programs/recursive-adt/deriving-eq.mlfp` corpus source as a
  parser-parity fixture plus a committed canonical parser projection that
  preserves module name `DerivingEq`.
- `test/conformance/mlfp/parser-parity/recursive-gadt/`: Added the exact
  `test/programs/recursive-adt/recursive-gadt.mlfp` corpus source as a
  parser-parity fixture plus a committed canonical parser projection that
  preserves module name `RecursiveGadt`.
- `test/conformance/mlfp/parser-parity/recursive-existential/`: Added the
  exact `test/programs/recursive-adt/recursive-existential.mlfp` corpus source
  as a parser-parity fixture plus a committed canonical parser projection that
  preserves module name `RecursiveExistential`.
- `test/programs/compiler-parser-parity/deriving-eq/`,
  `test/programs/compiler-parser-parity/recursive-gadt/`, and
  `test/programs/compiler-parser-parity/recursive-existential/`: Added thin
  parser-parity package roots that expose `sourceFile` and `sourceText`, then
  call the shared parser library instead of carrying fixture-local parser
  logic.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  Extended the shared parser-owned source parser with dynamic declaration
  sequences for the selected deriving, GADT, and existential recursive-ADT
  syntax families. The parser now renders rows from parsed source structure
  and actual module headers for the selected named corpus modules. It also
  removed the retired `Main`-only static recursive-ADT fallback recognizer.
- `test/ProgramParserParitySpec.hs`: Added source/expected/root constants,
  direct shared-parser assertions for all three named modules, aggregate
  generated-driver positive assertions, one batched malformed named
  recursive-ADT case-branch negative assertion, and round-specific
  shortcut/static guard phrases.
- `implementation_notes.md`, `CHANGELOG.md`, and
  `docs/mlfp-self-boot-readiness.md`: Added bounded round-335 parser-parity
  notes with explicit non-claims for full parser parity, checker/resolver,
  backend, compiler-package, platform, proof, driver, and self-boot progress.

### Tests
- `test/ProgramParserParitySpec.hs`: Verifies canonical projections and shared
  parser projections for `DerivingEq`, `RecursiveGadt`, and
  `RecursiveExistential`, includes the named recursive-ADT generated-driver
  batch positives, verifies malformed named recursive-ADT case-branch
  diagnostics, and audits against shortcut/static parser evidence.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`:
  PASS, 1 example, 0 failures. This was run after removing the static fallback
  to validate the repaired shortcut guard before the full focused rerun.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`:
  PASS, 50 examples, 0 failures, finished in 5086.2611 seconds.
- `git diff --check`: PASS, no whitespace errors.

### Notes
- An earlier full focused run failed only the shortcut guard because the shared
  parser still contained the retired static `parseRecursiveGadt` fallback. I
  removed that fallback and reran the guard plus the full focused group.
- I did not run `cabal build all && cabal test` because the approved plan's
  verification profile is focused and explicitly does not authorize full
  closeout gates for this non-closeout parser-parity slice.
- I did not run `./scripts/thesis-conformance-gate.sh` because this
  implementation updates only bounded parser-parity notes and makes no broader
  thesis/readiness claim.
- Controller recommendation: advance round-335 to review.
