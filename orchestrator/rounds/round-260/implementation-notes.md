### Changes Made
- `test/ProgramConformanceCorpusSpec.hs`: added the focused Hspec example `shared conformance corpus validates run-program search-path fixture`, plus narrow test-owned metadata handling for `search-paths`. The helper now derives `runProgramArgs` as the package root followed by ordered `--search-path` arguments from `fixture.meta`.
- `test/conformance/mlfp/README.md`: documented the new `search-paths` metadata field, `search-paths: none`, comma-separated ordered roots, and relative path resolution from `fixture.meta`.
- `test/conformance/mlfp/run-program/cross-module-let/fixture.meta`: added explicit `search-paths: none` for the existing fixture contract.
- `test/conformance/mlfp/run-program/search-path-package/fixture.meta`: added the minimal metadata for the approved search-path `run-program` tracer.
- `test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp`: added the copied main source fixture from `test/programs/packages/search-path-main/Main.mlfp`.
- `test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp`: added the copied library source fixture from `test/programs/packages/search-path-lib/SearchLib.mlfp`.
- `test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout`: added committed expected stdout containing exactly `2\n`.

### Tests
- `test/ProgramConformanceCorpusSpec.hs`: verifies that shared conformance corpus metadata drives public `run-program` execution for a package fixture and a search-path fixture, including exact committed stdout comparison.
- RED evidence: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program search-path fixture"'` was run after adding the focused example and metadata-derived `--search-path` helper, before adding the new search-path fixture data. It compiled and failed in the focused example because `test/conformance/mlfp/run-program/search-path-package/fixture.meta` did not exist: expected `True`, got `False`.
- GREEN evidence: the same focused command passed after adding the minimal search-path corpus data: 1 example, 0 failures.
- Regression evidence: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates run-program package fixture"'` passed for the existing cross-module-let fixture after adding `search-paths: none`: 1 example, 0 failures.
- Artifact check: `find test/conformance/mlfp/run-program -maxdepth 6 -type f | sort` listed only the existing cross-module-let fixture files and the new search-path package metadata, copied sources, and expected stdout.
- Artifact check: `rg -n 'fixture-id: search-path-run-program|package-root: roots/main|search-paths: roots/lib|command: run-program|expect: pass|normalization: none|stage-applicability: all|expected-stdout: expected/run-program.stdout' test/conformance/mlfp/run-program/search-path-package/fixture.meta` found the required metadata fields.
- Artifact check: `rg -n 'search-paths: none' test/conformance/mlfp/run-program/cross-module-let/fixture.meta test/conformance/mlfp/README.md` found the explicit no-search-path contract in both places.
- Artifact check: `printf '2\n' | diff -u - test/conformance/mlfp/run-program/search-path-package/expected/run-program.stdout` passed with no diff.
- Scope check: `diff -u test/programs/packages/search-path-main/Main.mlfp test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp` passed with no diff.
- Scope check: `diff -u test/programs/packages/search-path-lib/SearchLib.mlfp test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp` passed with no diff.
- Scope check: `git diff -- test/programs/packages/search-path-main test/programs/packages/search-path-lib` produced no output; the existing package fixtures were not changed.
- Closeout: `git diff --check` passed.
- Closeout: `cabal build all` passed.
- Closeout: `cabal test` passed: 2565 examples, 0 failures.
- Closeout: `./scripts/thesis-conformance-gate.sh` passed: thesis conformance anchors are green.

### Notes
The corpus helper remains test-owned and intentionally narrow. This round adds no fixture discovery, dynamic golden acceptance, regeneration tooling, production modules, parser/checker/backend/platform/driver/proof work, or horizontal corpus migration.

Manual audit: `ProgramConformanceCorpusSpec` derives the package root, ordered search paths, and expected stdout path from committed metadata, while command/status/normalization/stage/tags remain asserted to keep the fixture contract explicit. Controller-owned `orchestrator/state.json` was not edited by this implementation.
