### Selected Extraction
- Milestone: Filesystem Discovery And Dependency Graph
- Milestone id: `milestone-2`
- Direction id: `direction-2a-filesystem-discovery-graph`
- Extracted item id: `round-246-filesystem-discovery-graph`
- Roadmap id: `2026-05-17-00-mlfp-package-substrate-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001`

### Goal
Add the first filesystem-backed `.mlfp` package discovery path through the private package owner: discover one local package root's `.mlfp` source files, keep an explicit module-to-file graph and topological order, and reject missing or cyclic imports with stable source-path diagnostics. Prove that imports across discovered files still respect export visibility.

### Approach
Keep the semantic owner private to `MLF.Frontend.Program.Package`. This round may add owner-local helpers or owner-local submodules under `src/MLF/Frontend/Program/Package*`, but it must not add `src-public/` package facades, package search paths, cache policy, interface artifacts, package-manager scope, or CLI package mode.

Use a small IO boundary to discover and parse `.mlfp` files under one package root, then hand a `LocatedProgramPackage` to pure package graph/checking logic. The graph logic should derive a module-to-source-path map, reject duplicate module definitions across files, compute dependency order from imports, reject unknown imports, and report cycles before or while checking. Reuse existing parser spans and `ProgramDiagnostic` rendering so diagnostics identify the failing source file/import where possible.

### Steps
1. Re-check the current owner and call sites: `src/MLF/Frontend/Program/Package.hs`, `src/MLF/Frontend/Program/Check.hs`, `src/MLF/Frontend/Program/Resolve.hs`, `src/MLF/Frontend/Parse/Program.hs`, `src/MLF/Program/CLI.hs`, `test/ProgramPackageSpec.hs`, `test/ProgramSpec.hs`, `mlf2.cabal`, and `docs/architecture.md`.
2. Add focused failing coverage, preferably in a new `test/ProgramPackageDiscoverySpec.hs` registered in both `mlf2.cabal` and `test/Main.hs`. Cover:
   - a temporary package root with at least two `.mlfp` files where `Main` imports `Lib` and checking succeeds through `checkLocatedProgramPackage`;
   - deterministic module graph order, proving imported modules precede importers even when file discovery order differs;
   - a missing cross-file import that returns `ProgramUnknownImportModule` and renders the importing file path;
   - a two-file module cycle that returns `ProgramImportCycle` with a stable cycle/module diagnostic and source path;
   - an import of a hidden export from another discovered file that returns `ProgramImportNotExported` at the exposing item.
3. Implement the package-root discovery boundary under the package owner. It should take an explicit `PackageId` and root `FilePath`, deterministically collect `.mlfp` files for that one root, parse each file with `parseLocatedProgramWithFile`, and build a `LocatedProgramPackage` whose source units retain their file paths and spans.
4. Add pure package graph helpers owned by the package layer. They should expose enough internal information for tests and downstream owner code to inspect module ids, source paths, and topological dependency order without reimplementing graph policy in tests, CLI, or the resolver.
5. Route `checkProgramPackage` / `checkLocatedProgramPackage` through the package graph validation/order before projecting to the existing `Program` resolver/checker path. Preserve current trivial-package behavior and same-compilation-unit forward-import behavior where the graph defines it.
6. Improve diagnostics only as needed for this milestone: missing imports and import visibility should keep using existing source spans; cycles should identify a stable cycle/module and render a useful source file span when a located package is checked.
7. Keep `MLF.Program.CLI`, backend emission preparation, and `src-public/MLF/API.hs` / `src-public/MLF/Pipeline.hs` behavior unchanged unless a minimal internal adapter is required to compile. Do not introduce user-facing package commands in this round.
8. Update `docs/architecture.md` to say that one-root filesystem discovery and explicit package module graph/order now exist, while interfaces, search paths, cache/build graph policy, package CLI mode, stable ABI/linking, and compiler-in-`.mlfp` remain future work.
9. If new modules are added under `src/` or `test/`, register them in `mlf2.cabal`; if a new spec module is added, wire it into `test/Main.hs`.

### Verification
Run focused checks first:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program diagnostics"'
```

Then run hygiene and the behavior-changing full gate before approval:

```sh
git diff --check
cabal build all && cabal test
```

Manual review should confirm no `src-public/` package facade, search-path/cache/interface policy, package-manager scope, or durable one-file semantic path was added.

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-246/selection-record.json`
- `orchestrator/rounds/round-246/round-plan-record.json`
