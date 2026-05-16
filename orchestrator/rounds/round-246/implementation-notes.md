### Changes Made
- `src/MLF/Frontend/Program/Package.hs`: added one-root filesystem discovery for `.mlfp` source files, package-owned module graph helpers, deterministic dependency ordering, duplicate/missing/cycle validation, and ordered package-to-program projections.
- `src/MLF/Frontend/Program/Check.hs`: routed package checking through the package-owned graph validation/order before resolver/checker execution.
- `src/MLF/Frontend/Program/Types.hs`: attached import-cycle diagnostics to the cycle module span so located packages render a source path.
- `test/ProgramPackageDiscoverySpec.hs`: added focused filesystem discovery, dependency order, missing import, cycle, and cross-file export visibility coverage.
- `test/Main.hs` and `mlf2.cabal`: registered `ProgramPackageDiscoverySpec`.
- `docs/architecture.md`: documented the new one-root discovery and explicit module graph/order capability while keeping search paths, interfaces, cache/build graph policy, CLI package mode, ABI/linking, and public APIs out of scope.

### Tests
- `test/ProgramPackageDiscoverySpec.hs`: verifies discovery across files, imported-before-importer order independent of file order, missing import source-path diagnostics, cycle diagnostics, and import export-visibility diagnostics across files.
- `test/ProgramPackageSpec.hs`: existing package-owner trivial package path still passes.
- `test/ProgramSpec.hs`: existing diagnostics suite still passes.
- Focused commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'` passed: 5 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'` passed: 5 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program diagnostics"'` passed: 49 examples, 0 failures.
- Full gate:
  - `git diff --check` passed before full gate.
  - `cabal build all && cabal test` passed: full build completed, `mlf2-test` passed with 2445 examples, 0 failures.

### Notes
No `src-public/` package facade, CLI package command, search-path/cache/interface policy, package-manager scope, roadmap coordination edit, or `orchestrator/state.json` edit was introduced. The discovery boundary is one explicit local root owned by `MLF.Frontend.Program.Package`; existing one-file inputs remain trivial package source units rather than a second semantic mode.
