### Selected Extraction
- Milestone: Package Roots, Search Paths, Cache, And Build Graph
- Milestone id: milestone-4
- Direction id: direction-4a-package-build-graph-policy
- Extracted item id: round-248-package-build-graph-policy
- Roadmap id: 2026-05-17-00-mlfp-package-substrate-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001

### Goal
Define the local package build policy that sits on top of the completed package discovery graph and typed interface artifact contract: explicit package roots and ordered search paths, deterministic build graph nodes, and cache validation keyed by source/interface metadata. The round should make stale source and dependency-interface cache behavior fail closed with diagnostics naming the module or interface involved, without adding package-manager, remote-dependency, public API, or CLI migration scope.

### Approach
Work serially in the canonical round worktree. The selected code area spans `MLF.Frontend.Program.Package`, the private interface owner, checker integration, Cabal/test wiring, and architecture docs, so worker fan-out would create ownership overlap rather than useful isolation.

Use the existing owners as the base:

- `MLF.Frontend.Program.Package` already owns `PackageId`, `PackageModuleId`, source units, one-root discovery, graph validation, and ordered package projection.
- `MLF.Frontend.Program.Interface` already owns typed checked module interfaces with source paths, direct `PackageModuleId` dependencies, checked exports, data/classes, instances, and fail-closed validation.
- `docs/architecture.md` currently states that there is no package root/search path policy, persisted interface artifact, cache/build graph policy, stable `.mlfp` ABI, linker, or CLI package mode.

Add a private package-build policy surface under `src/MLF/Frontend/Program/`, preferably `MLF.Frontend.Program.BuildGraph`, and keep any package-root/search-path entrypoints package-owned. The new policy should use deterministic metadata derived from parsed source content and typed interface summaries. Do not rely on modification time, file size, hidden globals, or "if stale then reparse source" fallbacks as the correctness mechanism.

### Steps
1. Inspect the current package/interface/checker surfaces and tests before editing: `src/MLF/Frontend/Program/Package.hs`, `src/MLF/Frontend/Program/Interface.hs`, `src/MLF/Frontend/Program/Check.hs`, `test/ProgramPackageDiscoverySpec.hs`, `test/ProgramInterfaceSpec.hs`, `docs/architecture.md`, `mlf2.cabal`, and `test/Main.hs`.
2. Add focused failing tests in a new `test/ProgramPackageBuildGraphSpec.hs` under the Hspec description `MLF.Program package build graph policy`.
3. Cover explicit roots and search paths: create temp package roots where `Main` imports `Lib` from another local root, assert the ordered search path resolves deterministically, and assert omitting that root fails with a diagnostic tied to the importing module/source path.
4. Cover deterministic duplicate/root conflict handling: if the same module appears in more than one searched root, fail closed with the module name and enough source-path context for a reviewer to identify the conflicting source.
5. Cover build graph metadata: build graph nodes must include the package module id, source path, direct imports, deterministic source metadata, and direct dependency interface metadata in package graph order.
6. Cover cache invalidation: a cached interface for a module is valid only when its module id, source metadata, direct dependency ids, dependency interface metadata, and interface summary metadata still match the current package build graph.
7. Cover stale source behavior: change a dependency source while keeping module names/imports valid and assert the old cache entry is rejected for the changed module instead of silently reused.
8. Cover changed dependency behavior: change a dependency interface summary and assert importers that recorded the previous dependency-interface metadata are invalidated or rejected with a diagnostic naming the importer and dependency interface.
9. Cover malformed/stale interface behavior at the build graph boundary using the typed `ModuleInterface`/`PackageInterface` values from the prior milestone; keep malformed interfaces fail-closed and do not treat interfaces as a second typechecker authority.
10. Implement explicit local root/search path types and deterministic discovery from an ordered list of roots. Preserve the existing one-root helper as a thin adapter to the explicit search path if that keeps current tests stable.
11. Implement the private build graph/cache policy types and validation functions. Keep them internal to `src/` and do not add exports from `src-public/MLF/API.hs` or `src-public/MLF/Pipeline.hs`.
12. Wire new modules into `mlf2.cabal`, wire the new spec into both `mlf2.cabal` and `test/Main.hs`, and keep all production modules warning-free.
13. Update `docs/architecture.md` to replace the current "no package root/search path policy or cache/build graph policy" wording with the implemented private owner boundary, while still saying there is no package manager, remote dependency system, stable public ABI, linker, or CLI package mode.
14. Update `docs/mlfp-language-reference.md` only if user-facing package layout wording is needed to explain the new explicit local roots/search paths; do not claim CLI package mode or self-boot readiness.
15. Review public-surface tests and source-public imports to confirm the new build graph/cache policy remains private.

### Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package build graph policy"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`
- `git diff --check`
- Before approval for this behavior-changing round: `cabal build all && cabal test`

Manual checks:

- Confirm source/interface metadata is content- or summary-derived and deterministic, not mtime-only, size-only, or hidden global state.
- Confirm stale cache behavior fails closed and diagnostics name the failing module or interface artifact.
- Confirm CLI/package public API migration is not included in this round.
- Confirm no package-manager, remote dependency, linker, stable public ABI, or self-boot compiler implementation scope was added.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this file. They are the machine authority for lineage and worker scheduling.
