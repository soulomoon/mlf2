### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-263-conformance-check-program-fail-missing-import-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the first public failure fixture to the shared `.mlfp` conformance corpus. The fixture should migrate the existing user-visible `check-program` missing-import diagnostic behavior into `test/conformance/mlfp/check-program/missing-import/`, declare `expect: fail`, call the public `checkProgramArgs [packageRoot]` path, and compare the committed diagnostic output exactly.

This is implementation work, not status-only closeout. Milestone-2 is not complete yet because the current corpus has only pass fixtures and the README still says the tracer contract recognizes only `expect: pass`. This round should add one failure tracer and the narrow metadata/output contract needed for it; it must not claim milestone-2 completion.

### Approach
The implementer must load and use `/Users/ares/.agents/skills/tdd/SKILL.md`.

First public-interface behavior:

> A shared conformance corpus fixture declares `command: check-program` and `expect: fail` for a package root whose `Main.mlfp` imports a missing module, and the Haskell compiler test harness validates that fixture by calling the public `checkProgramArgs [packageRoot]` behavior and comparing the returned diagnostic text with a committed expected stderr file.

Focused RED test slice to write first:

Add one `ProgramConformanceCorpusSpec` example named `shared conformance corpus validates check-program missing-import failure fixture`. Extend only the test-owned fixture loader enough to distinguish `expect: pass` from `expect: fail` and compare `Left diagnostic` against a committed expected stderr file. Before adding the new fixture files, run the focused matcher and prove it fails because `test/conformance/mlfp/check-program/missing-import/fixture.meta` or `expected/check-program.stderr` is missing, not because the spec is unwired or does not compile.

Use this fixture layout:

```text
test/conformance/mlfp/check-program/missing-import/fixture.meta
test/conformance/mlfp/check-program/missing-import/src/Main.mlfp
test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr
```

Use the existing line-oriented `key: value` metadata contract, extended narrowly for failure output:

```text
fixture-id: missing-import-check-program
package-root: src
search-paths: none
command: check-program
expect: fail
normalization: none
stage-applicability: all
tags: package,public,diagnostic,missing-import,check
expected-stderr: expected/check-program.stderr
```

Use this source fixture:

```mlfp
module Main export (main) {
  import Missing;
  def main : Bool = true;
}
```

The committed expected stderr should reflect current public `checkProgramArgs` diagnostic rendering for that fixture, including the fixture-relative source path:

```text
test/conformance/mlfp/check-program/missing-import/src/Main.mlfp:2:10
error: unknown imported module `Missing`
```

Do not add `run-program` failure behavior, all diagnostics, directory-wide fixture discovery, actual-output roots, accept/bless/regenerate tooling, backend/native conformance, platform/driver behavior, or proof artifacts in this round.

### Steps
1. Reconfirm selected lineage from `selection-record.json` and the active rev-003 bundle. Confirm `milestone-2` remains `in-progress`, depends on completed `milestone-1`, and `direction-2a-conformance-corpus-migration` remains the selected direction.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle.
3. Write the RED test before adding the new failure fixture data:
   - add the example `shared conformance corpus validates check-program missing-import failure fixture`;
   - add a `FixtureExpectation` for `test/conformance/mlfp/check-program/missing-import/fixture.meta`;
   - introduce a small expected-result representation in the test helper, enough for `expect: pass` plus `expected-stdout` and `expect: fail` plus `expected-stderr`;
   - keep `command: check-program` dispatched through public `checkProgramArgs`;
   - keep the existing four pass examples green.
4. Prove RED with the focused command in the Verification section. The failure should be the missing new failure fixture metadata or committed stderr file.
5. Add the minimal migrated failure fixture:
   - add `src/Main.mlfp` with the missing `import Missing;` source above;
   - add `fixture.meta` with the fields above;
   - add committed expected stderr `expected/check-program.stderr` containing exactly the diagnostic text above.
6. Update `test/conformance/mlfp/README.md` only for the new narrow contract:
   - `expect` may now be `pass` or `fail`;
   - pass fixtures use `expected-stdout`;
   - fail fixtures use `expected-stderr`;
   - expected-output updates remain reviewed source changes;
   - ordinary test runs must not regenerate, bless, or dynamically accept new outputs.
7. Keep implementation test/corpus-owned and narrow. Do not change production compiler behavior unless the migration exposes a real existing bug, and if it does, record the bug and focused evidence clearly in `implementation-notes.md`.
8. Re-run the focused matcher and make the same example pass. If the public diagnostic text differs from the planned expected stderr, update the committed expected file only to match the observed current public behavior and record the observation command/output in `implementation-notes.md`; do not add dynamic acceptance.
9. Record RED and GREEN evidence, expected-stderr checks, existing corpus checks, changed files, and no-dynamic-golden audit in `implementation-notes.md`.

### Verification
Required focused TDD command:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program missing-import failure fixture"'
```

Run it once after writing the test and before adding the new fixture data; it must fail for the missing/incomplete failure fixture contract. Run it again after implementation; it must pass.

Required existing-corpus focused check:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates"'
```

Required artifact checks:

```bash
find test/conformance/mlfp -maxdepth 7 -type f | sort
```

```bash
rg -n 'fixture-id: missing-import-check-program|package-root: src|search-paths: none|command: check-program|expect: fail|normalization: none|stage-applicability: all|expected-stderr: expected/check-program.stderr' test/conformance/mlfp/check-program/missing-import/fixture.meta
```

```bash
diff -u /tmp/round-263-expected-stderr test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr
```

For the stderr diff above, create `/tmp/round-263-expected-stderr` with:

```text
test/conformance/mlfp/check-program/missing-import/src/Main.mlfp:2:10
error: unknown imported module `Missing`
```

```bash
rg -n 'module Main export \\(main\\)|import Missing;|def main : Bool = true;' test/conformance/mlfp/check-program/missing-import/src/Main.mlfp
```

```bash
rg -n 'expect: fail|expected-stderr|expected-stdout|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|writeFile|appendFile' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md test/conformance/mlfp/check-program/missing-import
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

- `implementation-notes.md` must state that `/Users/ares/.agents/skills/tdd/SKILL.md` was loaded and used.
- `implementation-notes.md` must record the RED focused command output summary and prove the failure was the missing/incomplete `check-program` failure fixture contract.
- `implementation-notes.md` must record the GREEN focused command output summary for the same matcher.
- The review must confirm all shared conformance examples pass through the existing-corpus focused check.
- The review must confirm fail fixtures compare committed `expected-stderr` and pass fixtures continue to compare committed `expected-stdout`.
- The review must confirm ordinary tests do not regenerate, bless, or dynamically accept expected output.
- The review must confirm the round did not change production compiler behavior unless a real migration-exposed bug is recorded with focused evidence.
- The review must confirm the round does not claim milestone-2 completion.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
