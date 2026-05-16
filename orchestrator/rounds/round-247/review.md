### Checks Run
- Command: `git status --short`
  Result: pass. Diff is scoped to the expected docs, Cabal/test wiring, checker/interface code, and round artifacts. No `orchestrator/state.json` edit is present.
- Command: `git diff --check`
  Result: pass. No whitespace errors.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'`
  Result: pass. 6 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'`
  Result: pass. 5 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program resolved symbol identities"'`
  Result: pass. 5 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
  Result: pass. 26 examples, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass. Full test suite passed with 2453 examples, 0 failures.
- Command: manual diff review of `src/MLF/Frontend/Program/Interface.hs`, `src/MLF/Frontend/Program/Check.hs`, `test/ProgramInterfaceSpec.hs`, `test/PublicSurfaceSpec.hs`, `mlf2.cabal`, `test/Main.hs`, `docs/architecture.md`, and `docs/mlfp-resolved-symbol-identities.md`.
  Result: pass. The implementation matches the round plan and selected roadmap milestone.

### Plan Compliance
- Step 1: met. The changed implementation was reviewed against the package/checker/types/resolver surfaces, docs, tests, Cabal wiring, and public-surface expectations called out by the plan.
- Step 2: met. `test/ProgramInterfaceSpec.hs` covers interface extraction for values, abstract types, concrete constructors, classes, methods, instances, source/dependency metadata, import visibility, hidden constructor rejection, malformed interface rejection, checked-module mismatch, exported identity ownership, and Prelude-owned opaque exports. `PublicSurfaceSpec` guards public exposure.
- Step 3: met. `MLF.Frontend.Program.Interface` is an internal owner for `ModuleInterface`, `PackageInterface`, fail-closed `ProgramInterfaceError`, extraction, lookup, validation, and diagnostics.
- Step 4: met. Interfaces are extracted from checked module facts plus `PackageModuleGraph` after module checking. They are not inputs to inference and do not repair failed checking.
- Step 5: met. `MLF.Frontend.Program.Check` now feeds prior imports through `ModuleInterface` accessors for exports, local data, and visible instances. The change adds no public facade or duplicate resolver.
- Step 6: met. Package validation checks graph/order agreement, source paths, direct dependencies, missing/unexpected modules, export owner identity, constructor owner identity, class method ownership, and instance origin/class namespace.
- Step 7: met. Public entrypoint signatures and `src-public` modules remain unchanged; trivial packages, one-root discovery, CLI helpers, and backend paths stayed on the same public surface.
- Step 8: met. `docs/architecture.md` documents the private interface owner and explicitly excludes persisted ABI/cache, package manager/search-path policy, CLI package mode, and second typechecker authority. `docs/mlfp-resolved-symbol-identities.md` records the interface-specific export identity invariant.
- Step 9: met. `MLF.Frontend.Program.Interface` and `ProgramInterfaceSpec` are registered in `mlf2.cabal`, and `ProgramInterfaceSpec` is wired into `test/Main.hs`.

### Decision
**APPROVED**

### Evidence
The integrated diff satisfies milestone-3's completion signal. The new private interface artifact records checked exports, local data/class summaries, instances, source paths, and direct `PackageModuleId` dependencies; package checking validates the resulting interface before accepting a checked package; and import visibility for prior modules now consumes the interface boundary.

Focused tests cover the selected task-specific acceptance path, including interface extraction, dependency metadata, malformed artifacts, export identity ownership, hidden constructor rejection, and public-surface privacy. Baseline hygiene and the full behavior gate passed. The full gate rewrote the generated Rust depfile path to the worktree absolute path during validation; that generated churn was restored before this review artifact was written.

Closeout classification: status-only. The implementation completes the existing milestone-3 contract without changing future coordination, milestone meaning, sequencing, extraction scope, verification policy, or retry policy. The controller can apply `milestone-3` status `pending -> done` through `milestone-3-status` and add the compact completion pointer through `milestone-3-completion`.
