### Selected Extraction
- Milestone: Interface Artifact Contract
- Milestone id: `milestone-3`
- Direction id: `direction-3a-interface-artifact-contract`
- Extracted item id: `round-247-interface-artifact-contract`
- Roadmap id: `2026-05-17-00-mlfp-package-substrate-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001`

### Goal
Introduce the private typed `.mlfp` module interface artifact boundary used by package imports and later separate compilation. The interface must summarize checked exports and dependency metadata for values, abstract types, constructors, classes, class methods, and instances, and package checking must validate import visibility through that interface path rather than peeking directly at source or ad hoc checked-module fields.

### Approach
Keep interfaces internal to the package/program layer. Add a private module such as `MLF.Frontend.Program.Interface` that owns typed interface artifacts and fail-closed validation. This is not a persisted ABI, cache format, package manager, search-path policy, public facade, or second typechecker authority.

The interface should be derived from already-checked module facts (`ModuleExports`, `DataInfo`, `ClassInfo`, `ValueInfo`, `InstanceInfo`) plus package graph facts (`PackageModuleId`, source path, direct dependencies). It may wrap existing checked summaries instead of inventing a parallel semantic model, but it must give imports one explicit boundary to consume. Malformed or stale typed artifacts should fail closed when module id, source path, dependency metadata, exported identity, or required dependency interface data disagrees with the current package graph/check result. Source freshness and cache invalidation policy remain milestone 4 work.

Use serial implementation. The selected scope cuts across `Package`, `Check`, shared program types, docs, and focused tests, so fan-out would create overlapping ownership.

### Steps
1. Re-check the current package and symbol surfaces before editing: `src/MLF/Frontend/Program/Package.hs`, `src/MLF/Frontend/Program/Check.hs`, `src/MLF/Frontend/Program/Types.hs`, `src/MLF/Frontend/Program/Resolve.hs`, `docs/mlfp-resolved-symbol-identities.md`, `docs/architecture.md`, `test/ProgramPackageDiscoverySpec.hs`, `test/ProgramPackageSpec.hs`, `test/ProgramSpec.hs`, `test/PublicSurfaceSpec.hs`, `mlf2.cabal`, and `test/Main.hs`.
2. Add focused failing coverage, preferably in a new `test/ProgramInterfaceSpec.hs` registered in both `mlf2.cabal` and `test/Main.hs`. Cover:
   - extracting an interface for a checked package module that exports a value, an abstract type without constructors, a concrete type with constructors, a class with a method, and an instance;
   - dependency metadata for a multi-file package, proving `Main` records a dependency on `Lib` through `PackageModuleId` and source path information from `PackageModuleGraph`;
   - import visibility succeeding through the interface path for exported values/types/classes/methods/instances;
   - hidden constructors or hidden types still rejected across files through the interface path;
   - malformed interface artifacts fail closed, for example wrong module id, wrong dependency list, exported symbol defining-module mismatch, or missing required dependency interface;
   - no public API exposure of package interfaces from `src-public/`.
3. Add the internal interface owner module. Suggested shape:
   - `ProgramInterface` or `PackageInterface` containing ordered `ModuleInterface` values;
   - `ModuleInterface` with `PackageModuleId`, `Maybe FilePath`, direct dependency module ids, exported value/type/class summaries, exported/visible instance summaries, and enough source/package identity metadata for validation;
   - `InterfaceValue`, `InterfaceType`, `InterfaceClass`, `InterfaceInstance`, or equivalent wrappers around checked summaries when that avoids duplicating typechecker facts;
   - `ProgramInterfaceError` for fail-closed validation errors.
4. Implement extraction from checked modules and the package graph. Build interfaces only after modules have checked; do not treat interface artifacts as an input to type inference or as a way to repair failed source checking.
5. Route import visibility in `MLF.Frontend.Program.Check` through the interface boundary. Existing helpers that currently consume prior `CheckedModule` fields or raw `ModuleExports` may keep using `ModuleExports` internally only through interface accessors such as `moduleInterfaceExports`, `moduleInterfaceData`, and `moduleInterfaceInstances`. Avoid a duplicate resolver or new public package facade.
6. Validate interface/package consistency before returning a successful checked package: module ids and order match the package graph, direct dependencies are present, source paths agree where known, exported symbol identities define the exporting module, abstract-type constructor visibility is preserved, and visible instance summaries refer only to allowed exported class/type identities.
7. Keep `checkProgram`, `checkLocatedProgram`, trivial packages, one-root discovery, CLI helpers, backend emission preparation, and `src-public/MLF/API.hs` / `src-public/MLF/Pipeline.hs` signatures unchanged unless a minimal internal compile adapter is required.
8. Update `docs/architecture.md` to record the new private interface owner and clarify that interfaces are typed internal checked summaries, not persisted cache files or public ABI. Update `docs/mlfp-resolved-symbol-identities.md` only if the implementation adds an interface-specific identity invariant that is not already documented.
9. If new modules are added under `src/` or `test/`, register them in `mlf2.cabal`; if a new spec module is added, wire it into `test/Main.hs`.

### Verification
Run focused checks first:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program resolved symbol identities"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'
```

Then run hygiene and the behavior-changing full gate before approval:

```sh
git diff --check
cabal build all && cabal test
```

Manual review should confirm interfaces are private typed checked summaries; import visibility passes through the interface boundary; malformed/stale typed interfaces fail closed; and no public facade, persisted cache policy, search-path policy, package-manager scope, CLI package mode, or second typechecking authority was added.

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-247/selection-record.json`
- `orchestrator/rounds/round-247/round-plan-record.json`
