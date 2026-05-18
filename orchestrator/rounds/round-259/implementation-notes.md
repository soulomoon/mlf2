### Changes Made
- `test/ProgramConformanceCorpusSpec.hs`: added the first shared conformance corpus Hspec example, `shared conformance corpus validates run-program package fixture`, plus narrow test-owned `key: value` metadata parsing. The test reads command, package root, pass/fail status, normalization, stage applicability, tags, and expected stdout from `fixture.meta`, runs the public `runProgramArgs` path, and compares committed stdout exactly.
- `test/Main.hs`: wired `ProgramConformanceCorpusSpec.spec` into the test suite.
- `mlf2.cabal`: registered `ProgramConformanceCorpusSpec` in the `mlf2-test` `other-modules` stanza.
- `test/conformance/mlfp/README.md`: documented the initial corpus contract, relative fixture path resolution, committed expected outputs, `normalization: none`, and the no dynamic accept/regenerate rule.
- `test/conformance/mlfp/run-program/cross-module-let/fixture.meta`: added the round-approved metadata for `cross-module-let-run-program`.
- `test/conformance/mlfp/run-program/cross-module-let/src/Core.mlfp`: added the migrated package source fixture.
- `test/conformance/mlfp/run-program/cross-module-let/src/Main.mlfp`: added the migrated package entrypoint fixture.
- `test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout`: added committed expected stdout containing exactly `1\n`.

### Tests
- `test/ProgramConformanceCorpusSpec.hs`: verifies that the shared conformance corpus metadata drives the public `run-program` package behavior and exact committed stdout comparison.
- RED evidence: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program package fixture"'` was run after wiring the spec and before adding `test/conformance/mlfp` fixture data. It compiled and failed in the focused example because `test/conformance/mlfp/run-program/cross-module-let/fixture.meta` did not exist: expected `True`, got `False`.
- GREEN evidence: the same focused command passed after adding the minimal conformance fixture: 1 example, 0 failures.
- Artifact check: `find test/conformance/mlfp -maxdepth 5 -type f | sort` listed only the README, fixture metadata, two source files, and expected stdout for `run-program/cross-module-let`.
- Artifact check: `rg -n 'fixture-id: cross-module-let-run-program|package-root: src|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout' test/conformance/mlfp/run-program/cross-module-let/fixture.meta` found the required metadata fields.
- Artifact check: `printf '1\n' | diff -u - test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout` passed with no diff.
- Scope check: `git diff -- test/programs/packages/cross-module-let` produced no output; the existing package fixture was not changed.
- Closeout: `git diff --check` passed.
- Closeout: `cabal build all` passed.
- Closeout: `cabal test` passed: 2564 examples, 0 failures.
- Closeout: `./scripts/thesis-conformance-gate.sh` passed: thesis conformance anchors are green.

### Notes
The harness support stays test-owned and intentionally narrow. It recognizes the single approved `run-program`/`pass`/`normalization: none` contract for this tracer and does not add production modules, fixture discovery, dynamic golden acceptance, regeneration tooling, diagnostics normalization, backend/native/platform/driver work, or self-boot proof artifacts.

Manual audit: ordinary tests read committed expected output only; there is no bless/update path. `ProgramConformanceCorpusSpec` derives the package root and expected output path from `fixture.meta`, while the command/status/normalization/stage/tags fields are asserted to keep the corpus contract explicit. No blockers remain for review.
