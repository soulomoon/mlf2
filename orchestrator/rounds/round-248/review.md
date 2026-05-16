### Checks Run
- Command: `git status --short`
  Result: pass. Diff is scoped to package build graph policy, package search-path discovery, docs, Cabal/test wiring, and round artifacts. No `orchestrator/state.json` edit is present.
- Command: `git diff --check`
  Result: pass. No whitespace errors.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package build graph policy"'`
  Result: pass. 8 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'`
  Result: pass. 5 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'`
  Result: pass. 6 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
  Result: pass. 26 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`
  Result: pass. 2 examples, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass. Full test suite passed with 2461 examples, 0 failures.
- Command: manual diff review of `src/MLF/Frontend/Program/BuildGraph.hs`, `src/MLF/Frontend/Program/Package.hs`, `test/ProgramPackageBuildGraphSpec.hs`, `test/PublicSurfaceSpec.hs`, `mlf2.cabal`, `test/Main.hs`, and `docs/architecture.md`.
  Result: pass. The implementation matches the round plan and selected roadmap milestone.

### Plan Compliance
- Step 1: met. The package, interface, checker-adjacent, discovery, public-surface, docs, Cabal, and test harness surfaces were reviewed against the plan and implementation notes.
- Step 2: met. `test/ProgramPackageBuildGraphSpec.hs` adds focused coverage under `MLF.Program package build graph policy`.
- Step 3: met. The search-path test discovers `Main` and `Lib` from ordered explicit local roots, and the missing-root diagnostic points at the importing source path.
- Step 4: met. Duplicate modules across searched roots fail closed through `ProgramPackageDiscoveryDuplicateModule`, including the duplicate module name and conflicting source paths.
- Step 5: met. Build graph nodes carry package module id, source path, direct imports, deterministic source metadata, and direct dependency interface metadata in package graph order.
- Step 6: met. Cache entries validate module id, source metadata, direct dependencies, dependency interface metadata, and the module's own interface summary metadata against the current graph/interface.
- Step 7: met. Changing dependency source rejects the changed module's stale cache entry instead of silently reusing it.
- Step 8: met. Changing dependency interface summary metadata rejects the importer cache entry and names both importer and dependency interface.
- Step 9: met. Malformed package interfaces are rejected at the build graph boundary through typed interface validation, not treated as a second typechecker authority.
- Step 10: met. `PackageRoot` and `PackageSearchPath` are explicit package-owned types, and the existing one-root discovery helper is now a thin adapter over the ordered search path.
- Step 11: met. `MLF.Frontend.Program.BuildGraph` is a private internal owner for deterministic build graph and cache validation policy.
- Step 12: met. `MLF.Frontend.Program.BuildGraph` and `ProgramPackageBuildGraphSpec` are registered in `mlf2.cabal`, and the spec is wired into `test/Main.hs`.
- Step 13: met. `docs/architecture.md` records explicit roots/search paths and the private build graph/cache owner while preserving no package-manager, remote dependency, linker, stable ABI, CLI package mode, or public package API scope.
- Step 14: met. No `docs/mlfp-language-reference.md` update was needed; the round did not add user-facing CLI package mode or self-boot claims.
- Step 15: met. `PublicSurfaceSpec` guards that the private build graph owner is not imported by `src-public`.

### Decision
**APPROVED**

### Evidence
The integrated round satisfies milestone-4's completion signal. Package roots and search paths are explicit and ordered; duplicate modules across roots fail closed with source-path context; build graph nodes include deterministic parsed-source metadata and direct dependency interface metadata; and cache validation rejects stale source, dependency-interface, and malformed-interface cases with module/interface names.

The source/interface metadata is derived from parsed module values and typed `ModuleInterface` summaries, not file modification time, file size, hidden global state, or source-reparse fallback. The diff stays private to internal package/build graph owners, adds no public API or CLI package mode, and does not introduce package-manager, remote-dependency, linker, stable public ABI, or self-boot compiler implementation scope.

Focused task-specific tests, public-surface tests, backend preparation coverage, diff hygiene, and the full Cabal gate all passed. The full gate rewrote the generated Rust depfile path to the worktree absolute path during validation; that generated churn was restored before this review artifact was written.

Closeout classification: status-only. The implementation completes the existing milestone-4 contract without changing future coordination, milestone meaning, sequencing, extraction scope, verification policy, retry policy, or compatibility posture. The controller can apply `milestone-4` status `pending -> done` through `milestone-4-status` and add the compact completion pointer through `milestone-4-completion`.
