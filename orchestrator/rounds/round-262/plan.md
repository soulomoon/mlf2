### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-262-conformance-check-program-search-path-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next `check-program` command-mode tracer to the shared `.mlfp` conformance corpus by migrating the existing public search-path package checking behavior into `test/conformance/mlfp/check-program/search-path-package/`. The fixture must declare a package root plus one ordered search path in metadata, call the public `checkProgramArgs [mainRoot, "--search-path", libRoot]` behavior, and compare committed expected stdout `OK\n`.

This round keeps milestone-2 in progress. It fills the command/search-path matrix opened by rounds 260 and 261 and must not widen into all commands, diagnostics migration, backend/native conformance, fixture discovery, production API changes, platform contracts, driver work, or self-boot proof records.

### Approach
The implementer must load and use `/Users/ares/.agents/skills/tdd/SKILL.md`.

First public-interface behavior:

> A shared conformance corpus fixture declares `command: check-program`, a package root, and an explicit ordered search path in metadata, and the Haskell compiler test harness validates that fixture by calling the public `checkProgramArgs [mainRoot, "--search-path", libRoot]` behavior and comparing committed stdout exactly.

Focused RED test slice to write first:

Add one `ProgramConformanceCorpusSpec` example named `shared conformance corpus validates check-program search-path fixture`. Reuse the existing metadata-driven fixture loader and command dispatch from rounds 260 and 261. Before adding the new `check-program/search-path-package` fixture files, run the focused matcher and prove it fails because `test/conformance/mlfp/check-program/search-path-package/fixture.meta` or its committed expected stdout is missing, not because the spec is unwired or does not compile.

Use this fixture layout:

```text
test/conformance/mlfp/check-program/search-path-package/fixture.meta
test/conformance/mlfp/check-program/search-path-package/roots/main/Main.mlfp
test/conformance/mlfp/check-program/search-path-package/roots/lib/SearchLib.mlfp
test/conformance/mlfp/check-program/search-path-package/expected/check-program.stdout
```

Use the existing line-oriented `key: value` metadata contract:

```text
fixture-id: search-path-check-program
package-root: roots/main
search-paths: roots/lib
command: check-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,search-path,cross-root-import,check
expected-stdout: expected/check-program.stdout
```

Do not convert the corpus to directory-wide discovery or a broad command matrix in this round. Keep this as one explicit `check-program` search-path example plus the existing three examples.

### Steps
1. Reconfirm selected lineage from `selection-record.json` and the active rev-003 bundle. Confirm `milestone-2` is `in-progress`, depends on completed `milestone-1`, and `direction-2a-conformance-corpus-migration` remains the selected direction.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle.
3. Write the RED test before adding the new fixture data:
   - add the example `shared conformance corpus validates check-program search-path fixture`;
   - add a `FixtureExpectation` for `test/conformance/mlfp/check-program/search-path-package/fixture.meta`;
   - expect `fixture-id: search-path-check-program`, `command: check-program`, `search-paths: roots/lib`, and tags `package,public,search-path,cross-root-import,check`;
   - keep `checkProgramArgs` as the public command path and derive both the package root and `--search-path` argv from metadata;
   - keep the existing `run-program` package, `run-program` search-path, and `check-program` package examples green.
4. Prove RED with the focused command in the Verification section. The failure should be the missing new `check-program` search-path fixture metadata or committed expected output.
5. Add the minimal migrated fixture:
   - copy `test/programs/packages/search-path-main/Main.mlfp` to `test/conformance/mlfp/check-program/search-path-package/roots/main/Main.mlfp`;
   - copy `test/programs/packages/search-path-lib/SearchLib.mlfp` to `test/conformance/mlfp/check-program/search-path-package/roots/lib/SearchLib.mlfp`;
   - add `fixture.meta` with the fields above;
   - add committed expected stdout `test/conformance/mlfp/check-program/search-path-package/expected/check-program.stdout` containing exactly `OK\n`.
6. Update `test/conformance/mlfp/README.md` only if needed to keep the documented tracer contract truthful for `check-program` with search paths. Preserve the no dynamic accept/regenerate rule.
7. Keep implementation test-owned and narrow. Do not add production modules, public API changes, fixture discovery across the whole tree, multiple expected streams, negative diagnostics, backend/native emission checks, platform/driver behavior, or self-boot proof artifacts.
8. Re-run the focused matcher and make the same example pass. If the migrated fixture does not produce `OK\n`, treat it as a behavior bug or fixture mismatch and stop for reviewer-visible notes instead of changing the expected output to match drift.
9. Record RED and GREEN evidence, source-copy checks, changed files, and no-dynamic-golden audit in `implementation-notes.md`.

### Verification
Required focused TDD command:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program search-path fixture"'
```

Run it once after writing the test and before adding the new fixture data; it must fail for the missing/incomplete `check-program` search-path fixture contract. Run it again after implementation; it must pass.

Required existing-corpus focused check:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates"'
```

Required artifact checks:

```bash
find test/conformance/mlfp -maxdepth 7 -type f | sort
```

```bash
rg -n 'fixture-id: search-path-check-program|package-root: roots/main|search-paths: roots/lib|command: check-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/check-program.stdout' test/conformance/mlfp/check-program/search-path-package/fixture.meta
```

```bash
printf 'OK\n' | diff -u - test/conformance/mlfp/check-program/search-path-package/expected/check-program.stdout
```

```bash
diff -u test/programs/packages/search-path-main/Main.mlfp test/conformance/mlfp/check-program/search-path-package/roots/main/Main.mlfp
```

```bash
diff -u test/programs/packages/search-path-lib/SearchLib.mlfp test/conformance/mlfp/check-program/search-path-package/roots/lib/SearchLib.mlfp
```

```bash
git diff -- test/programs/packages/search-path-main test/programs/packages/search-path-lib
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

- `implementation-notes.md` must state that `/Users/ares/.agents/skills/tdd/SKILL.md` was loaded and used.
- `implementation-notes.md` must record the RED focused command output summary and prove the failure was the missing/incomplete `check-program` search-path fixture contract.
- `implementation-notes.md` must record the GREEN focused command output summary for the same matcher.
- The review must confirm all shared conformance examples pass through the existing-corpus focused check.
- The review must confirm ordinary tests compare committed expected output and do not regenerate, bless, or dynamically accept new expected output.
- The review must confirm `test/programs/packages/search-path-main/` and `test/programs/packages/search-path-lib/` are unchanged and the new conformance source files match them byte-for-byte.
- The review must confirm the round did not change production compiler behavior or claim milestone-2 completion.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
