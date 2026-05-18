### Selected Extraction
- Milestone: Shared File-Based Conformance Corpus
- Milestone id: `milestone-2`
- Direction id: `direction-2a-conformance-corpus-migration`
- Extracted item id: `item-260-conformance-run-program-search-path-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next shared conformance-corpus tracer by migrating the existing public `run-program` search-path package behavior into `test/conformance/mlfp/`. The new fixture should declare a package root plus one ordered search path in metadata, run through the public `runProgramArgs` path, and compare committed expected stdout `2\n`.

This round keeps milestone-2 in progress. It expands the corpus contract narrowly for one search-path fixture and must not broaden into all command modes, fixture discovery, diagnostics, backend/native conformance, platform contracts, driver work, or self-boot proof records.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

First public-interface behavior:

> A shared conformance corpus fixture declares a `run-program` package root and an explicit ordered search path in metadata, and the Haskell compiler test harness validates that fixture by calling the public `runProgramArgs [mainRoot, "--search-path", libRoot]` behavior and comparing committed stdout exactly.

Focused RED test slice to write first:

`ProgramConformanceCorpusSpec` gets one new Hspec example named `shared conformance corpus validates run-program search-path fixture`. Add only the test-owned helper changes needed for this example to express metadata-driven `runProgramArgs` arguments. Before adding the new fixture files, run the focused matcher and prove it fails because the declared search-path conformance fixture metadata or expected output is absent, not because the spec is unwired or does not compile.

Use a small fixture layout:

```text
test/conformance/mlfp/run-program/search-path-package/fixture.meta
test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp
test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp
test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout
```

The metadata format remains the existing line-oriented `key: value` contract, extended only with an explicit `search-paths` field:

```text
fixture-id: search-path-run-program
package-root: roots/main
search-paths: roots/lib
command: run-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,search-path,cross-root-import
expected-stdout: expected/run-program.stdout
```

For this round, `search-paths` is a comma-separated ordered list resolved relative to the directory containing `fixture.meta`; this fixture uses exactly one search path. To avoid hidden Haskell-only defaults, also update the existing `cross-module-let` metadata and README contract to make the no-search-path case explicit as `search-paths: none`. Do not add JSON, TOML, YAML, dynamic golden acceptance, broad regeneration tooling, directory-wide discovery, or multi-command runners.

### Steps
1. Reconfirm the selected lineage from `selection-record.json` and active rev-003 bundle. Confirm `milestone-2` is `in-progress`, depends on completed `milestone-1`, and `direction-2a-conformance-corpus-migration` remains the selected direction.
2. Write the RED slice before adding the new search-path fixture data:
   - add one `ProgramConformanceCorpusSpec` example named `shared conformance corpus validates run-program search-path fixture`;
   - keep the public behavior check through `runProgramArgs`;
   - derive the main package root and `--search-path` argument from metadata, resolving paths relative to `fixture.meta`;
   - require `fixture-id`, `package-root`, `search-paths`, `command`, `expect`, `normalization`, `stage-applicability`, `tags`, and `expected-stdout` for the new fixture;
   - keep the previous `cross-module-let` test green by making its no-search-path metadata explicit rather than relying on an implicit default.
3. Prove RED with the focused command in the Verification section. The failure should be the missing `test/conformance/mlfp/run-program/search-path-package/fixture.meta` or committed expected output for the new fixture.
4. Add the minimal migrated fixture:
   - copy `test/programs/packages/search-path-main/Main.mlfp` to `test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp`;
   - copy `test/programs/packages/search-path-lib/SearchLib.mlfp` to `test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp`;
   - add `fixture.meta` with the fields above;
   - add committed expected stdout `test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout` containing exactly `2\n`.
5. Update `test/conformance/mlfp/README.md` only for the narrow metadata addition: document `search-paths: none` and comma-separated ordered `search-paths` values for this tracer contract, relative path resolution, and the continued no dynamic accept/regenerate rule.
6. Keep implementation test-owned and narrow. Do not add production modules, public API changes, fixture discovery across the whole tree, support for multiple commands, diagnostic normalization, backend/native conformance, platform/driver behavior, or self-boot proof artifacts.
7. Re-run the focused matcher and make the same example pass. If the migrated fixture does not produce `2\n`, treat it as a behavior bug or fixture mismatch and stop for reviewer-visible notes rather than updating the expected output to match drift.
8. Record RED and GREEN evidence, source-copy checks, changed files, and no-dynamic-golden audit in `implementation-notes.md`.

### Verification
Required focused TDD command:

```bash
cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program search-path fixture"'
```

Run it once after writing the test and before adding the search-path fixture data; it must fail for the missing/incomplete fixture contract. Run it again after implementation; it must pass.

Required artifact checks:

```bash
find test/conformance/mlfp/run-program -maxdepth 6 -type f | sort
```

```bash
rg -n 'fixture-id: search-path-run-program|package-root: roots/main|search-paths: roots/lib|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout' test/conformance/mlfp/run-program/search-path-package/fixture.meta
```

```bash
rg -n 'search-paths: none' test/conformance/mlfp/run-program/cross-module-let/fixture.meta test/conformance/mlfp/README.md
```

```bash
printf '2\n' | diff -u - test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout
```

```bash
diff -u test/programs/packages/search-path-main/Main.mlfp test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp
```

```bash
diff -u test/programs/packages/search-path-lib/SearchLib.mlfp test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp
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

- Confirm `ProgramConformanceCorpusSpec` derives `runProgramArgs` arguments from `fixture.meta`, including the `--search-path` argument, rather than hardcoding the new fixture's CLI argv.
- Confirm ordinary tests compare committed expected output and do not regenerate, bless, or dynamically accept new expected output.
- Confirm the round does not rewrite `test/programs/packages/search-path-main/` or `test/programs/packages/search-path-lib/`.
- Confirm the round keeps the existing `cross-module-let` conformance fixture passing after adding explicit `search-paths: none`.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
