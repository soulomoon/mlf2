### Checks Run
- Command: `sed -n '1,220p' /Users/ares/.agents/skills/tdd/SKILL.md`
  Result: pass; reviewed the required public-behavior, vertical RED -> GREEN -> refactor TDD discipline before evaluating the implementation notes.
- Command: `sed -n '1,220p' orchestrator/rounds/round-261/plan.md` and `sed -n '1,220p' orchestrator/rounds/round-261/implementation-notes.md`
  Result: pass; loaded the round plan and implementation evidence for item-261-conformance-check-program-package-tracer.
- Command: `jq -e . orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json`
  Result: pass; active rev-003 roadmap metadata is valid JSON and contains the milestone-2 anchors used for closeout.
- Command: `git diff --stat` and `git diff -- test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md`
  Result: pass; reviewed the integrated diff. The change is limited to the shared conformance harness, conformance README, the new check-program fixture, round artifacts, and controller-owned state.
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program package fixture"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program"'`
  Result: pass; 2 examples, 0 failures.
- Command: `find test/conformance/mlfp -maxdepth 7 -type f -print | sort`
  Result: pass; the new files are confined to `test/conformance/mlfp/check-program/cross-module-let/`.
- Command: `rg -n 'fixture-id: cross-module-let-check-program|package-root: src|search-paths: none|command: check-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/check-program.stdout' test/conformance/mlfp/check-program/cross-module-let/fixture.meta`
  Result: pass; required metadata fields are present.
- Command: `printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout`
  Result: pass; committed expected stdout is exactly `OK\n`.
- Command: `diff -u test/programs/packages/cross-module-let/Core.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Core.mlfp` and `diff -u test/programs/packages/cross-module-let/Main.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Main.mlfp`
  Result: pass; copied fixture sources match byte-for-byte.
- Command: `git diff -- test/programs/packages/cross-module-let`
  Result: pass; the existing package fixture was not rewritten.
- Command: `rg -n 'checkProgramArgs|runProgramArgs|command: check-program|command: run-program|writeFile|appendFile|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|expected-stdout' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp`
  Result: pass; public dispatch and committed expected-stdout references are present, with no dynamic golden accept/regenerate path.
- Command: `rg -n 'ProgramConformanceCorpusSpec' mlf2.cabal test/Main.hs`
  Result: pass; existing test module wiring remains present.
- Command: `git diff --check`
  Result: pass.
- Command: `cabal build all`
  Result: pass.
- Command: `cabal test`
  Result: pass; 2566 examples, 0 failures.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; thesis conformance anchors are green.
- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass after excluding generated churn; the build rewrote only the absolute worktree path in this generated depfile, and it was restored to the checked-in path.

### Plan Compliance
- item-261-conformance-check-program-package-tracer: met. The new fixture is under `test/conformance/mlfp/check-program/cross-module-let/`, with `command: check-program`, `package-root: src`, `search-paths: none`, `expect: pass`, and committed expected stdout.
- TDD requirement: met. The implementation notes record a focused RED failure for the missing check-program fixture metadata, followed by GREEN on the focused check-program matcher, matching the required vertical public-behavior cycle from `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Public interface behavior: met. `ProgramConformanceCorpusSpec` imports `checkProgramArgs` and `runProgramArgs`; `command: check-program` dispatches to `checkProgramArgs (fixtureArgs fixture)`, while existing `run-program` fixtures continue through `runProgramArgs`.
- Metadata-driven package root and output: met. The harness reads `package-root`, `search-paths`, `command`, and `expected-stdout` from metadata, then compares the public command result with the committed expected stdout.
- No dynamic golden acceptance: met. The grep audit found no write/append/accept/bless/regenerate/update path in the conformance harness or fixture tree.
- Existing package fixture preservation: met. `test/programs/packages/cross-module-let/` has no diff, and the copied `Core.mlfp` and `Main.mlfp` files match byte-for-byte.
- Scope control: met. The implementation stays within the conformance corpus/harness and README. It does not add production behavior, command discovery, diagnostics migration, backend/native coverage, or self-hosting claims.
- Roadmap closeout: status-only. The round completes the selected item and does not change future coordination semantics, TDD requirements, milestone meanings, dependencies, or staged order. Milestone-2 remains in-progress.

### Decision
**APPROVED**

### Evidence
The integrated round satisfies the selected rev-003 item narrowly. The focused public-interface tests prove both the new `check-program` package tracer and the pre-existing `run-program` fixtures. The committed expected stdout is exactly `OK\n`, fixture sources are copied without drift from the existing package fixture, and the grep audit found no dynamic golden acceptance path.

Full required gates passed: `git diff --check`, `cabal build all`, `cabal test` with 2566 examples and 0 failures, and `./scripts/thesis-conformance-gate.sh`. Generated build churn was limited to the Rust runtime depfile's absolute worktree path and was excluded from the payload.
