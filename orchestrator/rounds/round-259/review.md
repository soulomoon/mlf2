### Checks Run
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program package fixture"'`
  Result: pass; 1 example, 0 failures.

- Command: `find test/conformance/mlfp -maxdepth 5 -type f | sort`
  Result: pass; listed only the corpus README, `fixture.meta`, `src/Core.mlfp`, `src/Main.mlfp`, and `expected/run-program.stdout` for `run-program/cross-module-let`.

- Command: `rg -n 'fixture-id: cross-module-let-run-program|package-root: src|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout' test/conformance/mlfp/run-program/cross-module-let/fixture.meta`
  Result: pass; all required metadata fields were present.

- Command: `printf '1\n' | diff -u - test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout`
  Result: pass; committed expected stdout is exactly `1\n`.

- Command: `git diff --check`
  Result: pass; no whitespace errors reported.

- Command: `cabal build all`
  Result: pass.

- Command: `cabal test`
  Result: pass; 2564 examples, 0 failures.

- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; thesis obligations, claims validation, Phi/Omega rows, A6 regressions, Phase 3/7 gates, ga' stability, translatable presolution, Phi soundness, and expansion minimality passed. Final line: `[thesis-gate] PASS: thesis conformance anchors are green`.

- Command: `jq -e '.schema_version == "roadmap-view-v1" and .roadmap_revision == "rev-002" and (.milestones[] | select(.milestone_id == "milestone-2") | .status == "pending" and .status_anchor == "milestone-2-status") and (.anchors["milestone-2-status"].selector == "### [pending] 2. Shared File-Based Conformance Corpus") and (.anchors["milestone-2-completion"].selector == "#### Completion Pointers: milestone-2")' orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002/roadmap-view.json`
  Result: pass; printed `true`.

- Command: `jq -e '.roadmap_id == "2026-05-18-00-full-self-boot-end-to-end-roadmap" and .roadmap_revision == "rev-002" and .roadmap_dir == "orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002" and (.active_rounds[] | select(.round_id == "round-259" and .stage == "review"))' orchestrator/state.json`
  Result: pass; printed `true`.

- Manual check: inspected `test/ProgramConformanceCorpusSpec.hs`, `test/conformance/mlfp/README.md`, the new fixture metadata/source/expected files, `mlf2.cabal`, `test/Main.hs`, `selection-record.json`, `round-plan-record.json`, `implementation-notes.md`, and active rev-002 roadmap files.
  Result: pass; the test uses public `runProgramArgs`, derives package root and expected stdout from metadata, compares committed expected output, has no dynamic accept/regenerate path, and stays test-owned.

- Manual check: compared `test/conformance/mlfp/run-program/cross-module-let/src/Core.mlfp` and `src/Main.mlfp` against `test/programs/packages/cross-module-let/`.
  Result: pass; copied source matches, and `git diff -- test/programs/packages/cross-module-let` produced no output.

### Plan Compliance
- Selected lineage: met. `selection-record.json`, `round-plan-record.json`, `orchestrator/state.json`, and active `roadmap-view.json` agree on roadmap `2026-05-18-00-full-self-boot-end-to-end-roadmap`, revision `rev-002`, milestone `milestone-2`, direction `direction-2a-conformance-corpus-migration`, and item `item-259-conformance-run-program-package-tracer`.
- TDD cycle: met with credible evidence. `implementation-notes.md` records the focused RED run after wiring the spec and before adding fixture data; the failure was the missing `fixture.meta`, not compile or wiring failure. Reviewer reran the GREEN focused matcher and it passed.
- Public-interface behavior: met. `ProgramConformanceCorpusSpec` calls `runProgramArgs [fixturePackageRoot fixture]` and asserts `Right expected`.
- Metadata and expected output contract: met. Metadata declares command, package root, pass status, normalization, stage applicability, tags, and expected stdout. The harness resolves `package-root` and `expected-stdout` relative to `fixture.meta` and asserts the other declared fields.
- No dynamic golden acceptance: met. The harness reads committed expected stdout only; no bless/update/regeneration behavior was added.
- Test wiring: met. `ProgramConformanceCorpusSpec` is registered in `mlf2.cabal` and imported/run from `test/Main.hs`.
- Existing fixture preservation: met. `test/programs/packages/cross-module-let/` has no diff.
- Scope discipline: met. No production modules, public APIs, parser/checker/resolver/backend/platform/driver/proof behavior, or broad fixture discovery were added. The worktree also has the controller-owned `orchestrator/state.json` active-round diff; I did not edit it.

### Roadmap Closeout
Approved closeout mode: `status-only`.

Selectors are valid against active rev-002. The status change should move `milestone-2` from `pending` to `in-progress` through anchor `milestone-2-status`. A compact completion pointer under `milestone-2-completion` is justified because round-259 completed the first shared conformance corpus tracer, but this does not satisfy the full milestone completion signal and must not mark milestone-2 done. No history entry is included because active rev-002 `roadmap-view.json` exposes no roadmap-history anchor.

### Decision
**APPROVED**

### Evidence
The integrated result creates the first vertical tracer bullet for the shared `.mlfp` conformance corpus. It migrates the existing cross-module package behavior into `test/conformance/mlfp/run-program/cross-module-let/` with visible fixture metadata and committed stdout `1\n`, then validates the public `run-program` behavior through `runProgramArgs`.

The implementation preserves current behavior, keeps the existing package fixture untouched, avoids dynamic golden acceptance, and leaves milestone-2 in progress rather than complete. All plan-required focused, artifact, full, and thesis gates passed.
