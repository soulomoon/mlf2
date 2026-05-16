### Selected Extraction
- Milestone: CLI And Public Entry Point Migration
- Milestone id: milestone-5
- Direction id: direction-5a-cli-api-package-migration
- Extracted item id: round-249-cli-api-package-migration
- Roadmap id: 2026-05-17-00-mlfp-package-substrate-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001

### Goal
Make the user-facing `.mlfp` entrypoints exercise the package substrate instead of the old one-file path. `run-program`, `emit-backend`, `emit-native`, and the narrow public check/runtime adapters should accept multi-file local packages through the package owner, while existing file inputs continue as trivial packages. The round must not add package-manager, remote dependency, linker, broad public lowering API, fixture-migration, or compiler-in-`.mlfp` scope.

### Approach
Work serially. This migration crosses `app/Main.hs`, `MLF.Program.CLI`, `MLF.Backend.Emission.Prepare`, public `MLF.Pipeline` exports, focused tests, and CLI/docs wording; splitting it would create overlapping behavior and documentation ownership.

Keep `MLF.Program.CLI` as the command/file orchestration owner. Route semantic preparation through package-owned adapters: `MLF.Frontend.Program.Package`, `MLF.Frontend.Program.Prelude`, `MLF.Frontend.Program.Check`, `MLF.Frontend.Program.Run`, and `MLF.Backend.Emission.Prepare`. Do not expose `MLF.Frontend.Program.Interface` or `MLF.Frontend.Program.BuildGraph` from public modules.

Use this CLI grammar unless implementation evidence shows a smaller equivalent is clearer:

- `mlf2 check-program <file-or-root> [--search-path <root> ...]`
- `mlf2 run-program <file-or-root> [--search-path <root> ...]`
- `mlf2 emit-backend <file-or-root> [--search-path <root> ...]`
- `mlf2 emit-native <file-or-root> [--search-path <root> ...]`

Interpret an existing file path as a trivial package source unit and an existing directory path as the primary package root. Additional `--search-path` roots extend the ordered local `PackageSearchPath`. If a path is neither an existing file nor directory, fail with a user-facing diagnostic naming that path. Preserve the existing two-argument file behavior.

### Steps
1. Inspect the current surfaces before editing: `app/Main.hs`, `src/MLF/Program/CLI.hs`, `src/MLF/Backend/Emission/Prepare.hs`, `src/MLF/Frontend/Program/Package.hs`, `src/MLF/Frontend/Program/Run.hs`, `src/MLF/Frontend/Program/Check.hs`, `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`, `test/ProgramPackageSpec.hs`, `test/ProgramPackageDiscoverySpec.hs`, `test/ProgramPackageBuildGraphSpec.hs`, `test/BackendEmissionPrepareSpec.hs`, `test/BackendLLVMSpec.hs`, `test/PublicSurfaceSpec.hs`, `README.md`, `docs/mlfp-language-reference.md`, and `docs/architecture.md`.
2. Add focused failing tests for `MLF.Program CLI package migration`, preferably in a new `test/ProgramCliPackageSpec.hs`, and wire the spec into `mlf2.cabal` and `test/Main.hs`.
3. Cover CLI package input resolution: file input still becomes a Prelude-prepended trivial package; directory input discovers a package root; additional `--search-path` roots discover imports from secondary roots in deterministic order; malformed option shapes fail with `programCliUsage` or a precise diagnostic.
4. Cover `check-program` on a multi-file package root and on a file input. The success path may print a small stable success string or empty output, but failures must render existing package/check diagnostics with source paths.
5. Cover `run-program` on a multi-file package root where `Main` imports a value/type from another file or search-path root and returns the expected runtime text. Keep the existing single-file fixture assertion.
6. Cover `emit-backend` on a multi-file package root by asserting LLVM is emitted for `Main__main` and validating it through existing LLVM test support where available.
7. Cover `emit-native` package mode at least at the emitted LLVM boundary. If linked native execution is already used by a nearby focused test and remains cheap, include one package-root native execution smoke; otherwise keep native execution to existing backend-native tests and assert package-mode native LLVM emits a process entrypoint.
8. Add package-aware runtime adapters in `MLF.Frontend.Program.Run`, such as `runProgramPackage`, `runLocatedProgramPackage`, `runProgramPackageOutput`, and `runLocatedProgramPackageOutput`, implemented by `checkProgramPackage` / `checkLocatedProgramPackage` rather than flattening back through `checkProgram` / `checkLocatedProgram`.
9. Add package-aware backend preparation in `MLF.Backend.Emission.Prepare`, such as `prepareBackendEmissionFromProgramPackage` and `prepareBackendEmissionFromLocatedPackage`, reusing the existing Prelude pruning step after package checking. Keep `prepareBackendEmissionFromSource` as the file/trivial-package adapter.
10. Refactor `MLF.Program.CLI` around a small package input resolver that isolates IO path inspection, package discovery, Prelude injection, and CLI argument parsing. Keep command rendering and user-facing error text in this module; keep semantic package checking/preparation in package/backend owners.
11. Update `app/Main.hs` to route `check-program`, `run-program`, `emit-backend`, and `emit-native` through the package-aware CLI helpers and to allow the optional `--search-path` arguments.
12. Add narrow public package-facing adapters to `MLF.Pipeline`: export package identity/root/search-path and located package discovery/check/run functions needed by downstream users. Do not export private interface/build graph/cache types, backend lowering helpers, or LLVM rendering APIs from `MLF.API` or `MLF.Pipeline`.
13. Extend `PublicSurfaceSpec` to prove the new public package check/runtime adapters work on a multi-file package and that `MLF.Frontend.Program.Interface` / `MLF.Frontend.Program.BuildGraph` remain private implementation details.
14. Update `docs/architecture.md` so `MLF.Program.CLI` remains the CLI orchestration owner and package semantics stay package-owned. Update `README.md` and `docs/mlfp-language-reference.md` for the new CLI package usage without claiming fixture migration, stable ABI, package manager support, or self-boot readiness.
15. Review for stale one-file semantics: existing file entrypoints may remain only as trivial package adapters, and there should be no independent resolver/checker/runtime path that bypasses the package owner for `.mlfp` user-facing commands or public check/runtime adapters.

### Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI package migration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner|MLF.Program package filesystem discovery|MLF.Program package build graph policy"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI file entrypoint|package"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
- `git diff --check`
- Before approval for this behavior-changing round: `cabal build all && cabal test`

Manual checks:

- Confirm `run-program`, `emit-backend`, `emit-native`, `check-program`, and public package adapters use package checking/preparation for multi-file inputs.
- Confirm existing single-file examples are preserved only through trivial package handling, not a separate durable semantic mode.
- Confirm package diagnostics name the failing source file/module/root/search path where applicable.
- Confirm no package manager, remote dependency, linker, public backend lowering API, fixture migration, stable ABI, or self-boot compiler implementation scope was added.
- Classify support layers in implementation notes: source checking, interpreter/runtime, backend/native, object code, and package build mode.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this file. They are the machine authority for lineage and worker scheduling.
