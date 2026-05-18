### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-259-conformance-run-program-package-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-002`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002`

### Goal
Create the first vertical tracer bullet for the shared `.mlfp` conformance corpus by migrating one existing public package behavior into `test/conformance/mlfp/`: the static cross-module package fixture currently covered by `ProgramCliPackageSpec` must run through the public `run-program` behavior and compare against committed expected stdout `1\n`.

This round establishes a small corpus contract, metadata, fixture source, committed expected output, and one focused harness assertion. It must preserve current compiler behavior by default and must not implement broad parser, checker, resolver, backend, platform, driver, or self-boot proof work.

### Approach
Keep the round serial with `worker_mode: none`.

Use the `tdd` skill because this is behavior-changing implementation work. The first public-interface behavior is:

> A shared conformance corpus fixture declares a package root and `run-program` command in metadata, and the Haskell compiler test harness validates that fixture by running the public `run-program` path and comparing the committed expected stdout exactly.

The focused RED slice to write first is a single Hspec example named `shared conformance corpus validates run-program package fixture`, wired through a new `ProgramConformanceCorpusSpec` test module. Before adding the corpus fixture files, make this focused test compile and fail because the declared conformance fixture/metadata/expected output is absent or incomplete. Record that RED failure in `implementation-notes.md`, then add only enough corpus data and harness support to make this one behavior pass.

Use a deliberately small fixture layout:

```text
test/conformance/mlfp/README.md
test/conformance/mlfp/run-program/cross-module-let/fixture.meta
test/conformance/mlfp/run-program/cross-module-let/src/Core.mlfp
test/conformance/mlfp/run-program/cross-module-let/src/Main.mlfp
test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout
```

The metadata format for this tracer bullet should be a plain, line-oriented `key: value` contract documented in the corpus README, with only the fields needed by this fixture:

```text
fixture-id: cross-module-let-run-program
package-root: src
command: run-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,cross-module,let-polymorphism
expected-stdout: expected/run-program.stdout
```

Do not introduce JSON, TOML, YAML, dynamic golden acceptance, broad regeneration tooling, or hidden Haskell-only defaults. The test may use Haskell test-support parsing for this minimal metadata, but command mode, package root, expected file, pass/fail status, normalization, stage applicability, and tags must be visible in the fixture metadata. Recognize only `normalization: none` in this round.

### Steps
1. Reconfirm the selected lineage from `selection-record.json` and the active rev-002 bundle. Confirm `milestone-2` depends only on completed `milestone-1` and that `direction-2a-conformance-corpus-migration` is the selected direction.
2. Write the first RED test before adding the conformance fixture data:
   - add `test/ProgramConformanceCorpusSpec.hs`;
   - wire it into `test/Main.hs`;
   - register it in the `mlf2.cabal` `mlf2-test` `other-modules` stanza;
   - include one example named `shared conformance corpus validates run-program package fixture`;
   - have the example load `test/conformance/mlfp/run-program/cross-module-let/fixture.meta`, derive the package root and expected stdout path from metadata, call `runProgramArgs [packageRoot]`, and assert exact output.
3. Prove RED with the focused command in the Verification section. The failure must be because the conformance fixture contract/data is not present yet, not because the spec module is unwired or does not compile.
4. Add the minimal corpus fixture:
   - copy the existing static package behavior into `test/conformance/mlfp/run-program/cross-module-let/src/Core.mlfp` and `src/Main.mlfp`;
   - add `fixture.meta` with the fields named above;
   - add committed expected stdout `test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout` containing exactly `1\n`.
5. Add `test/conformance/mlfp/README.md` documenting the minimal corpus contract for this first fixture: metadata fields, package-root resolution relative to the fixture directory, committed expected outputs, `normalization: none`, no dynamic acceptance during ordinary tests, and explicit review for expected-output updates.
6. Keep harness support narrow and test-owned. Do not add production modules, public API, fixture discovery across the whole tree, multi-command runners, diagnostic normalization, backend/native conformance, or self-boot proof records in this round.
7. Re-run the focused test and make the same example pass. If the migrated fixture does not produce `1\n`, treat it as a behavior bug or fixture mismatch and stop for reviewer-visible notes instead of updating the expected output to match a changed result.
8. Record RED and GREEN evidence, changed files, and any manual no-dynamic-golden audit in `implementation-notes.md`.

### Verification
Required focused TDD commands:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program package fixture"'
```

Run once after writing the test and before adding fixture data; it must fail for the missing/incomplete conformance fixture contract. Run again after implementation; it must pass.

Required artifact checks:

```bash
find test/conformance/mlfp -maxdepth 5 -type f | sort
```

```bash
rg -n 'fixture-id: cross-module-let-run-program|package-root: src|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout' test/conformance/mlfp/run-program/cross-module-let/fixture.meta
```

```bash
printf '1\n' | diff -u - test/conformance/mlfp/run-program/cross-module-let/expected/run-program.stdout
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

Manual checks:

- Confirm `ProgramConformanceCorpusSpec` derives command behavior and expected output from `fixture.meta`, not from hidden hardcoded defaults.
- Confirm ordinary tests compare committed expected output and do not regenerate, bless, or accept new expected output dynamically.
- Confirm this round does not remove or rewrite the existing `test/programs/packages/cross-module-let/` fixture or broaden into search-path, diagnostics, backend/native, platform, driver, or proof work.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
