### Changes Made
- `src/MLF/Frontend/Program/Package.hs`: added explicit `PackageRoot`/`PackageSearchPath` types, ordered search-path discovery, and fail-closed duplicate-module detection that reports the conflicting source paths.
- `src/MLF/Frontend/Program/BuildGraph.hs`: added the private package build graph/cache policy owner with deterministic parsed-source metadata, typed interface summary metadata, direct dependency interface metadata, cache-entry construction, and fail-closed cache validation.
- `mlf2.cabal`: registered the new private `MLF.Frontend.Program.BuildGraph` module and the new focused spec.
- `test/ProgramPackageBuildGraphSpec.hs`: added focused coverage for explicit roots/search paths, duplicate root conflicts, build graph node metadata, cache entry validation, stale source rejection, changed dependency-interface rejection, and malformed interface rejection at the build graph boundary.
- `test/Main.hs`: wired `ProgramPackageBuildGraphSpec` into the Hspec suite.
- `test/PublicSurfaceSpec.hs`: extended the public-surface guard so the new private build graph owner is not imported by `src-public`.
- `docs/architecture.md`: updated package/module/interface ownership guidance to include explicit local roots/search paths and the private build graph/cache policy while preserving no package-manager, remote-dependency, linker, stable ABI, CLI package mode, or public API scope.

### Tests
- `test/ProgramPackageBuildGraphSpec.hs`: verifies the new package build graph policy and cache invalidation rules.
- `test/ProgramPackageDiscoverySpec.hs`: verifies existing package filesystem discovery behavior still passes.
- `test/ProgramInterfaceSpec.hs`: verifies typed interface artifact behavior still passes.
- `test/PublicSurfaceSpec.hs`: verifies the new build graph owner remains private.
- `test/BackendEmissionPrepareSpec.hs`: verifies backend emission preparation still passes.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package build graph policy"'`: passed, 8 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'`: passed, 5 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'`: passed, 6 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`: passed, 26 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`: passed, 2 examples.
- `git diff --check`: passed.
- `cabal build all && cabal test`: passed, 2461 examples.

### Notes
The build graph/cache policy is private to `src/` and does not add public API, CLI package mode, package-manager scope, persisted interface file format, remote dependency handling, linker behavior, or stable `.mlfp` ABI. Source metadata is derived from parsed module content and interface metadata is derived from typed `ModuleInterface` summaries; cache validation rejects stale entries rather than silently reparsing or falling back to file mtime/size checks.
