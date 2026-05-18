### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-264-conformance-corpus-milestone-2-status-closeout`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Perform a status-only closeout audit for milestone-2. The current corpus has the required representative shared oracle pieces after rounds 259 through 263: fixture metadata, per-command expected stdout/stderr files, focused harness checks, a documented reviewed-source expected-output update policy with no ordinary regeneration/blessing path, migrated public pass and fail fixtures, and no dynamic golden acceptance or ad hoc skips.

This round should produce evidence for the reviewer to approve milestone-2 status changing from `in-progress` to `done`. It must not add implementation work, new fixtures, production compiler changes, semantic roadmap changes, or new verification meaning.

### Approach
This is a status-only closeout round, not behavior-changing implementation. The TDD workflow is exempt for this round under rev-003 because no behavior implementation is selected.

Validate the milestone-2 completion signal directly against current files and approved prior-round evidence:

- rounds 259 through 263 have approved review records under `milestone-2` / `direction-2a-conformance-corpus-migration`;
- `test/conformance/mlfp/` contains public pass fixtures for `run-program` and `check-program`, including package and search-path cases;
- `test/conformance/mlfp/` contains the public `check-program` failure fixture for missing-import diagnostics;
- every fixture has `fixture.meta` with package root, search paths, command, pass/fail status, normalization, stage applicability, behavioral tags, and the correct expected-output field;
- pass fixtures compare committed `expected-stdout`; fail fixtures compare committed `expected-stderr`;
- `test/conformance/mlfp/README.md` records the expected-output policy: expected updates are reviewed source changes, ordinary tests must not regenerate/bless/dynamically accept outputs, and actual outputs may only go to an explicitly selected actual-output root in a later round;
- no harness path writes, blesses, regenerates, dynamically accepts, skips, or marks fixtures pending.

If any check fails, stop and record the blocker in `implementation-notes.md`. Do not patch the corpus in this closeout round; a failing audit means the controller should plan another implementation round or request a semantic roadmap update.

### Steps
1. Reconfirm selected lineage from `selection-record.json`, `round-plan-record.json`, `orchestrator/state.json`, and rev-003 `roadmap-view.json`.
2. Load prior round evidence for rounds 259 through 263 and confirm each selected item was approved with successful focused conformance tests, `cabal build all`, `cabal test`, and thesis gate evidence.
3. Audit `test/conformance/mlfp/README.md`, `test/ProgramConformanceCorpusSpec.hs`, and all `test/conformance/mlfp/*/*/fixture.meta` files against the milestone-2 completion signal.
4. Run the focused corpus validation commands in the Verification section. This is closeout validation, not RED/GREEN TDD.
5. Run full closeout checks. Because this status-only closeout gates the next roadmap stage, include the full build/test/thesis gates even though no behavior change is planned.
6. Write `implementation-notes.md` with the exact command results and a completion-signal checklist. It must explicitly state that no code, fixture, README, production compiler, roadmap, or controller state edits were made during implementation.
7. Reviewer should approve only if the evidence proves milestone-2 is complete enough for the current rev-003 completion signal. The approving `review-record.json` should request status-only closeout:
   - `status_changes`: change `milestone-2` from `in-progress` to `done` using anchor `milestone-2-status`;
   - `completion_pointers`: add a compact pointer under `milestone-2-completion` naming round-264 as the status-only conformance corpus closeout audit;
   - `semantic_update_required_reason`: `null`.

### Verification
Required lineage and roadmap checks:

```bash
jq -e '.roadmap_id == "2026-05-18-00-full-self-boot-end-to-end-roadmap" and .roadmap_revision == "rev-003" and ([.milestones[] | select(.milestone_id == "milestone-2" and .status == "in-progress" and (.direction_ids | index("direction-2a-conformance-corpus-migration")))] | length == 1)' orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json
```

```bash
jq -e '.decision == "approved" and .milestone_id == "milestone-2" and .direction_id == "direction-2a-conformance-corpus-migration"' orchestrator/rounds/round-259/review-record.json orchestrator/rounds/round-260/review-record.json orchestrator/rounds/round-261/review-record.json orchestrator/rounds/round-262/review-record.json orchestrator/rounds/round-263/review-record.json
```

Required focused corpus checks:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates"'
```

```bash
find test/conformance/mlfp -maxdepth 7 -type f | sort
```

```bash
rg -n 'fixture-id:|package-root:|search-paths:|command:|expect:|normalization: none|stage-applicability: all|tags:|expected-stdout:|expected-stderr:' test/conformance/mlfp/*/*/fixture.meta
```

```bash
rg -n 'ExpectStdout|ExpectStderr|shouldMatchFixture|checkProgramArgs|runProgramArgs|expected-stdout|expected-stderr|expect: pass|expect: fail' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md test/conformance/mlfp
```

Required no-dynamic-golden and no-skip audit:

```bash
rg -n 'writeFile|appendFile|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|skip|pending' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp
```

The command above may find README policy text for `actual-output`; it must not find test-harness writes, accept/bless/regenerate behavior, skips, or pending fixtures.

Required expected-output spot checks:

```bash
printf '1\n' | diff -u - test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout
```

```bash
printf '2\n' | diff -u - test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout
```

```bash
printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout
```

```bash
printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/search-path-package/expected/check-program.stdout
```

```bash
printf 'test/conformance/mlfp/check-program/missing-import/src/Main.mlfp:2:10\nerror: unknown imported module `Missing`\n' | diff -u - test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr
```

Required closeout checks:

```bash
git diff --check
```

```bash
cabal build all
```

```bash
cabal test
```

```bash
./scripts/thesis-conformance-gate.sh
```

Reviewer-visible evidence requirements:

- `implementation-notes.md` must state this was status-only closeout validation and TDD was not required because no behavior implementation was selected.
- `implementation-notes.md` must list the five current shared conformance examples and their pass/fail/output-stream classifications.
- `implementation-notes.md` must include command results for focused corpus validation, expected-output spot checks, no-dynamic-golden/no-skip audit, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.
- Review must confirm there were no edits to production compiler code, conformance fixtures, corpus README, roadmap files, or `orchestrator/state.json` during this closeout implementation.
- Review must confirm milestone-2 can move to `done` via status-only closeout without changing future coordination, milestone meaning, direction meaning, sequencing, parallel lanes, extraction scope, verification meaning, or retry policy.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial status-only closeout round with no worker fan-out.
