### Changes Made
- `test/ProgramConformanceCorpusSpec.hs`: added the focused public-interface example `shared conformance corpus validates check-program package fixture`, extended the test-owned conformance fixture loader to read `command`, and dispatched `command: check-program` through public `checkProgramArgs [packageRoot]` while preserving existing `run-program` examples through `runProgramArgs`.
- `test/conformance/mlfp/README.md`: documented the narrow corpus contract for the two recognized commands, committed expected stdout, package root, search paths, and the no dynamic acceptance/regeneration rule.
- `test/conformance/mlfp/check-program/cross-module-let/fixture.meta`: added the minimal check-program package fixture metadata for `cross-module-let-check-program`.
- `test/conformance/mlfp/check-program/cross-module-let/src/Core.mlfp`: copied the existing package fixture source from `test/programs/packages/cross-module-let/Core.mlfp`.
- `test/conformance/mlfp/check-program/cross-module-let/src/Main.mlfp`: copied the existing package fixture source from `test/programs/packages/cross-module-let/Main.mlfp`.
- `test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout`: added the committed expected stdout `OK\n`.
- `orchestrator/rounds/round-261/implementation-notes.md`: recorded implementer evidence for the round.

### Tests
- RED focused check before adding the new fixture files:
  - Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program package fixture"'`
  - Result: failed after compiling and wiring the new Hspec example because the new check-program fixture contract/data was missing. Hspec reported 1 example, 1 failure at `test/ProgramConformanceCorpusSpec.hs:86:12`, `expected: True but got: False`, from the missing `test/conformance/mlfp/check-program/cross-module-let/fixture.meta`.
- GREEN focused check after adding the minimal fixture:
  - Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program package fixture"'`
  - Result: passed, 1 example, 0 failures.
- Existing run-program conformance examples:
  - Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program"'`
  - Result: passed, 2 examples, 0 failures.
- Artifact/source-copy/no-dynamic-golden checks:
  - `find test/conformance/mlfp -maxdepth 7 -type f | sort`: confirmed the new check-program fixture files are present alongside the existing run-program fixtures.
  - `rg -n 'fixture-id: cross-module-let-check-program|package-root: src|search-paths: none|command: check-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/check-program.stdout' test/conformance/mlfp/check-program/cross-module-let/fixture.meta`: confirmed the required metadata contract fields.
  - `printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout`: passed, confirming committed expected stdout is exactly `OK\n`.
  - `diff -u test/programs/packages/cross-module-let/Core.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Core.mlfp`: passed, confirming source copy.
  - `diff -u test/programs/packages/cross-module-let/Main.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Main.mlfp`: passed, confirming source copy.
  - `git diff -- test/programs/packages/cross-module-let`: no output, confirming existing package fixtures were not changed.
  - `rg -n 'checkProgramArgs|runProgramArgs|command: check-program|command: run-program|writeFile|appendFile|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|expected-stdout' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp`: found only the intended public argument dispatch, metadata fields, expected-stdout documentation, and README prohibition language; no write/bless/regeneration harness was added.
- Closeout checks:
  - `git diff --check`: passed.
  - `cabal build all`: passed.
  - `cabal test`: passed, 2566 examples, 0 failures.
  - `./scripts/thesis-conformance-gate.sh`: passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
Implementation stayed test/corpus-only. No production modules, parser/checker/backend/platform/driver/proof code, broad discovery tooling, dynamic golden acceptance, or existing `test/programs/packages/cross-module-let` fixture changes were made.

`orchestrator/state.json` was already modified in the worktree and remains controller-owned; it was not edited for this implementation. The round input artifacts `plan.md`, `round-plan-record.json`, and `selection-record.json` were present as untracked controller inputs and were not edited.
