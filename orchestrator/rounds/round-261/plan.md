### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-261-conformance-check-program-package-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the first `check-program` command-mode tracer to the shared `.mlfp` conformance corpus. The new fixture should migrate the existing public cross-module package checking behavior into `test/conformance/mlfp/check-program/cross-module-let/`, declare `command: check-program` in metadata, call the public `checkProgramArgs` path, and compare committed expected stdout `OK\n`.

This round keeps milestone-2 in progress. It adds one command-mode tracer and must not widen into all commands, negative diagnostics, backend/native conformance, fixture discovery, production API changes, platform contracts, driver work, or self-boot proof records.

### Approach
The implementer must load and use `/Users/ares/.agents/skills/tdd/SKILL.md`.

First public-interface behavior:

> A shared conformance corpus fixture declares `command: check-program` for a package root in metadata, and the Haskell compiler test harness validates that fixture by calling the public `checkProgramArgs [packageRoot]` behavior and comparing committed stdout exactly.

Focused RED test slice to write first:

Add one `ProgramConformanceCorpusSpec` example named `shared conformance corpus validates check-program package fixture`. Update only the test-owned helper needed to dispatch a metadata-declared command to `checkProgramArgs` or the existing `runProgramArgs` path. Before adding the new `check-program` fixture files, run the focused matcher and prove it fails because `test/conformance/mlfp/check-program/cross-module-let/fixture.meta` or its committed expected stdout is missing, not because the spec is unwired or does not compile.

Use this fixture layout:

```text
test/conformance/mlfp/check-program/cross-module-let/fixture.meta
test/conformance/mlfp/check-program/cross-module-let/src/Core.mlfp
test/conformance/mlfp/check-program/cross-module-let/src/Main.mlfp
test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout
```

Use the existing line-oriented `key: value` metadata contract, extended only so the harness recognizes `command: check-program` for this fixture:

```text
fixture-id: cross-module-let-check-program
package-root: src
search-paths: none
command: check-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,cross-module,let-polymorphism,check
expected-stdout: expected/check-program.stdout
```

Do not convert the corpus to a multi-command discovery harness in this round. Keep this as one explicit example plus the existing two `run-program` examples.

### Steps
1. Reconfirm selected lineage from `selection-record.json` and active rev-003 bundle. Confirm `milestone-2` is `in-progress` and `direction-2a-conformance-corpus-migration` remains the selected direction.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle.
3. Write the RED test before adding the new fixture data:
   - import `checkProgramArgs` in `ProgramConformanceCorpusSpec`;
   - add the example `shared conformance corpus validates check-program package fixture`;
   - derive the package root and expected stdout path from metadata;
   - dispatch `command: check-program` to `checkProgramArgs` and preserve existing `run-program` dispatch through `runProgramArgs`;
   - keep the existing `run-program` package and search-path examples green.
4. Prove RED with the focused command in the Verification section. The failure should be the missing new `check-program` fixture metadata or expected output.
5. Add the minimal migrated fixture:
   - copy `test/programs/packages/cross-module-let/Core.mlfp` to the new fixture `src/Core.mlfp`;
   - copy `test/programs/packages/cross-module-let/Main.mlfp` to the new fixture `src/Main.mlfp`;
   - add `fixture.meta` with the fields above;
   - add committed expected stdout `test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout` containing exactly `OK\n`.
6. Update `test/conformance/mlfp/README.md` only to document that the current tracer contract recognizes `run-program` and `check-program`, both with `expect: pass`, committed expected stdout, `normalization: none`, and no dynamic accept/regenerate path.
7. Keep implementation test-owned and narrow. Do not add production modules, public API changes, fixture discovery across the whole tree, multiple expected streams, negative diagnostics, backend/native emission checks, platform/driver behavior, or self-boot proof artifacts.
8. Re-run the focused matcher and make the same example pass. If the migrated fixture does not produce `OK\n`, treat it as a behavior bug or fixture mismatch and stop for reviewer-visible notes instead of changing the expected output to match drift.
9. Record RED and GREEN evidence, source-copy checks, changed files, and no-dynamic-golden audit in `implementation-notes.md`.

### Verification
Required focused TDD command:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program package fixture"'
```

Run it once after writing the test and before adding the new fixture data; it must fail for the missing/incomplete `check-program` fixture contract. Run it again after implementation; it must pass.

Required artifact checks:

```bash
find test/conformance/mlfp -maxdepth 7 -type f | sort
```

```bash
rg -n 'fixture-id: cross-module-let-check-program|package-root: src|search-paths: none|command: check-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/check-program.stdout' test/conformance/mlfp/check-program/cross-module-let/fixture.meta
```

```bash
printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/cross-module-let/expected/check-program.stdout
```

```bash
diff -u test/programs/packages/cross-module-let/Core.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Core.mlfp
```

```bash
diff -u test/programs/packages/cross-module-let/Main.mlfp test/conformance/mlfp/check-program/cross-module-let/src/Main.mlfp
```

```bash
rg -n 'checkProgramArgs|runProgramArgs|command: check-program|command: run-program|writeFile|appendFile|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|expected-stdout' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp
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

- `implementation-notes.md` must record the RED focused command output summary and prove the failure was the missing/incomplete `check-program` fixture contract.
- `implementation-notes.md` must record the GREEN focused command output summary for the same matcher.
- The review must confirm the existing `run-program` conformance examples still pass.
- The review must confirm ordinary tests compare committed expected output and do not regenerate, bless, or dynamically accept new expected output.
- The review must confirm `test/programs/packages/cross-module-let/` is unchanged and the new conformance source files match it byte-for-byte.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
