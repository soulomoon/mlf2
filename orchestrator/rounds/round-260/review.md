### Checks Run
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program search-path fixture"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program package fixture"'`
  Result: pass; 1 example, 0 failures.
- Command: `find test/conformance/mlfp/run-program -maxdepth 6 -type f -print | sort`
  Result: pass; listed the existing cross-module-let fixture files plus only the new search-path package fixture metadata, copied sources, and committed expected stdout.
- Command: `rg -n "fixture-id: search-path-run-program|package-root: roots/main|search-paths: roots/lib|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout" test/conformance/mlfp/run-program/search-path-package/fixture.meta`
  Result: pass; required metadata fields resolve in the new fixture.
- Command: `rg -n "search-paths: none" test/conformance/mlfp/run-program/cross-module-let/fixture.meta test/conformance/mlfp/README.md`
  Result: pass; no-search-path metadata is explicit for the existing fixture and documented.
- Command: `printf '2\n' | diff -u - test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout`
  Result: pass; expected stdout is exactly `2\n`.
- Command: `diff -u test/programs/packages/search-path-main/Main.mlfp test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp`
  Result: pass; copied main fixture matches the existing package fixture.
- Command: `diff -u test/programs/packages/search-path-lib/SearchLib.mlfp test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp`
  Result: pass; copied library fixture matches the existing package fixture.
- Command: `git diff -- test/programs/packages/search-path-main test/programs/packages/search-path-lib`
  Result: pass; old `test/programs` search-path fixtures are unchanged.
- Command: `rg -n "writeFile|appendFile|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|expected-stdout|runProgramArgs|search-path|package-root" test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp`
  Result: pass; the spec reads committed expected stdout and no dynamic accept/regenerate path is present.
- Command: `rg -n "ProgramConformanceCorpusSpec" mlf2.cabal test/Main.hs`
  Result: pass; existing spec module remains wired in both the cabal stanza and test driver.
- Command: `git diff --check`
  Result: pass.
- Command: `cabal build all`
  Result: pass.
- Command: `cabal test`
  Result: pass; 2565 examples, 0 failures.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; thesis conformance anchors are green.

### Plan Compliance
- Selected lineage: met. `selection-record.json`, `round-plan-record.json`, and active rev-003 bundle agree on `milestone-2`, `direction-2a-conformance-corpus-migration`, and `item-260-conformance-run-program-search-path-tracer`.
- TDD requirement: met. The active rev-003 roadmap requires `/Users/ares/.agents/skills/tdd/SKILL.md`; implementation notes record a vertical RED -> GREEN slice where the focused public-interface test first failed on missing `search-path-package/fixture.meta`, then passed after adding the minimal fixture.
- Public behavior: met. `ProgramConformanceCorpusSpec` calls `runProgramArgs (fixtureRunProgramArgs fixture)`, deriving the package root and ordered `--search-path` argv from `fixture.meta`.
- Fixture metadata and expected output: met. The new fixture declares `package-root: roots/main`, `search-paths: roots/lib`, `command: run-program`, `expect: pass`, `normalization: none`, and committed `expected/run-program.stdout` containing `2\n`.
- Dynamic golden policy: met. Ordinary tests compare committed stdout with `readFile`; no accept, bless, regenerate, or hidden production behavior was added.
- Scope: met. The round is limited to the shared conformance corpus test helper, README metadata contract, existing fixture metadata, and the new copied fixture files. No production modules, command discovery, broader corpus migration, backend/native, platform, driver, or self-boot proof work was added.
- Old fixture preservation: met. `test/programs/packages/search-path-main/` and `test/programs/packages/search-path-lib/` have no diff, and the copied conformance files compare byte-for-byte against them.
- Generated runtime dependency churn: met after exclusion. Full gates rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` only by changing absolute worktree paths; that is generated build churn and was restored out of the review payload.

### Decision
**APPROVED**

### Evidence
The integrated result satisfies `item-260-conformance-run-program-search-path-tracer` and stays narrow. The reviewer reran the focused conformance examples, artifact/source-copy checks, no-dynamic-golden grep, diff hygiene, `cabal build all`, full `cabal test`, and the thesis conformance gate successfully. Closeout is status-only: record round-260 under `milestone-2-completion` and leave `milestone-2` in-progress.
