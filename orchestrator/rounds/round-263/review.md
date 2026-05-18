### Checks Run
- Command: `sed -n '1,220p' /Users/ares/.agents/skills/tdd/SKILL.md`
  Result: pass; loaded the required TDD skill before reviewing RED/GREEN evidence. The implementation notes follow the vertical public-behavior RED -> GREEN -> refactor discipline: one missing-import `check-program` fixture first, then the minimal corpus harness support, then broader checks.
- Command: `sed -n '1,220p' AGENTS.md && sed -n '1,220p' orchestrator/roles/reviewer.md && sed -n '1,260p' orchestrator/round-finalization-schema.md && sed -n '1,220p' orchestrator/project-contract.md`
  Result: pass; loaded repo, reviewer, finalization, and project conformance-corpus contracts.
- Command: `jq . orchestrator/state.json >/tmp/round-263-state.json && jq . orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json >/tmp/round-263-roadmap-view.json`
  Result: pass; active roadmap bundle is `2026-05-18-00-full-self-boot-end-to-end-roadmap` `rev-003`.
- Command: `sed -n '1,260p' orchestrator/rounds/round-263/plan.md && sed -n '1,260p' orchestrator/rounds/round-263/implementation-notes.md && jq . orchestrator/rounds/round-263/selection-record.json && jq . orchestrator/rounds/round-263/round-plan-record.json`
  Result: pass; selected item is `item-263-conformance-check-program-fail-missing-import-tracer` under `milestone-2` / `direction-2a-conformance-corpus-migration`.
- Command: `git diff --stat && git diff -- test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md && find test/conformance/mlfp/check-program/missing-import -maxdepth 4 -type f -print | sort`
  Result: pass; changed files are narrow: shared conformance harness, corpus README, and the new missing-import fixture. No production compiler modules changed.
- Command: `printf 'test/conformance/mlfp/check-program/missing-import/src/Main.mlfp:2:10\nerror: unknown imported module \`Missing\`\n' | diff -u - test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr`
  Result: pass; committed failure oracle is exactly the expected `checkProgramArgs` diagnostic text with trailing newline.
- Command: `rg -n 'fixture-id|package-root|search-paths|command|expect|expected-stderr' test/conformance/mlfp/check-program/missing-import/fixture.meta && rg -n 'import Missing|def main' test/conformance/mlfp/check-program/missing-import/src/Main.mlfp`
  Result: pass; metadata uses `command: check-program`, `expect: fail`, `expected-stderr: expected/check-program.stderr`, and the source imports missing module `Missing`.
- Command: `rg -n 'writeFile|appendFile|bless|regenerate|accept|expected-stdout|expected-stderr|ExpectStdout|ExpectStderr|checkProgramArgs|runProgramArgs' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md test/conformance/mlfp/check-program/missing-import/fixture.meta`
  Result: pass; ordinary tests compare committed expected output and provide no regenerate, bless, or dynamic accept path.
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program missing-import failure fixture"'`
  Result: pass; 1 example, 0 failures. This is the focused public fail fixture.
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates"'`
  Result: pass; 5 examples, 0 failures. All shared conformance examples pass through the focused corpus check.
- Command: `git diff --check`
  Result: pass; no whitespace errors.
- Command: `cabal build all`
  Result: pass.
- Command: `cabal test`
  Result: pass; `2568 examples, 0 failures`.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; thesis obligations, claim validation, and thesis conformance anchors are green.
- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass after cleanup; validation temporarily rewrote this generated depfile to the round worktree absolute path, and I restored it to the checked-in `/Volumes/src/mlf4/...` path. It is excluded from the payload.

### Plan Compliance
- `item-263-conformance-check-program-fail-missing-import-tracer`: met. The round adds `test/conformance/mlfp/check-program/missing-import/` with metadata, source, and committed `expected/check-program.stderr`.
- TDD requirement: met. The implementation notes record a credible RED failure for the new focused example before the fixture existed, followed by GREEN on the focused example and the full shared corpus matcher. The tested behavior is public `checkProgramArgs` output, not an internal helper.
- Public fail oracle: met. `expect: fail` routes to `expected-stderr` and asserts `actual == Left expected`; pass fixtures still route to `expected-stdout` and assert `actual == Right expected`.
- Shared corpus behavior: met. Existing run/check pass fixtures remain in the same focused corpus check and the reviewer-rerun matcher passed all 5 examples.
- Scope control: met. The diff is limited to corpus harness/docs and a new fixture; no parser, checker, resolver, backend, native runtime, platform, driver, or proof behavior was changed.
- Dynamic golden policy: met. No ordinary test path writes, blesses, regenerates, or dynamically accepts expected output.
- Generated churn: met. `runtime/mlfp_io/target/release/libmlfp_io.d` was generated build churn from validation and is clean after restoration.
- Roadmap closeout: met. `milestone-2` remains `in-progress`; this round only justifies an item completion pointer at the existing `milestone-2-completion` anchor.

### Decision
**APPROVED**

### Evidence
The integrated result satisfies the round plan narrowly. `ProgramConformanceCorpusSpec` now uses metadata to select pass/fail expectations: `expected-stdout` is compared against `Right stdout`, and `expected-stderr` is compared against `Left stderr` from the public CLI helper path. `command: check-program` dispatches through `checkProgramArgs`; existing `run-program` fixtures continue through `runProgramArgs`.

The new missing-import fixture commits both the source and exact diagnostic oracle. The expected stderr is:

```text
test/conformance/mlfp/check-program/missing-import/src/Main.mlfp:2:10
error: unknown imported module `Missing`
```

Reviewer-rerun gates passed: focused missing-import matcher, full shared conformance corpus matcher, `git diff --check`, `cabal build all`, `cabal test` with 2568 examples and 0 failures, and `./scripts/thesis-conformance-gate.sh`. The requested status-only closeout is represented in `review-record.json` as `roadmap_closeout.mode = "status-only"` because `review-record-v3` has no separate `review_type` field.
