### Selected Extraction
- Milestone: Package Module Owner
- Milestone id: `milestone-1`
- Direction id: `direction-1a-package-module-owner`
- Extracted item id: `round-245-package-module-owner-trivial-package`
- Roadmap id: `2026-05-17-00-mlfp-package-substrate-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001`

### Goal
Create the first private `.mlfp` package/module owner and make today's parsed or located one-file programs enter the checker/runtime preparation path as trivial package inputs. This round must not implement filesystem discovery, package roots, interface artifacts, cache policy, new CLI package modes, or public package facades.

### Approach
Current HEAD evidence:

- `src/MLF/Frontend/Syntax/Program.hs` defines `Program` as a list of modules and `LocatedProgram` as a `Program` plus a span index; there is no package identity, source-unit shape, or package/module owner.
- `src/MLF/Frontend/Program/Resolve.hs` owns same-compilation-unit module ordering today: `resolveProgram` topologically sorts modules already present in one `Program`, then resolves imports from prior module exports.
- `src/MLF/Frontend/Program/Check.hs` still starts from `checkProgram :: Program -> Either ProgramError CheckedProgram`, directly resolving the raw program before checking.
- `src/MLF/Frontend/Program/Prelude.hs`, `src/MLF/Program/CLI.hs`, and `src/MLF/Backend/Emission/Prepare.hs` prepare current file inputs by concatenating the built-in Prelude with the parsed one-file program and then calling the current located-program checker/runtime path.
- `docs/architecture.md` explicitly documents the current `.mlfp` module boundary as same-compilation-unit only, with no persisted interface artifact, import loader, stable `.mlfp` ABI, or linker.
- `mlf2.cabal` registers the current `MLF.Frontend.Program.*` modules but has no package owner module yet.

Use a serial implementation. Add one private package owner under `MLF.Frontend.Program` and route existing in-memory single-file behavior through it while preserving current public API signatures.

Expected owner shape:

- Add `src/MLF/Frontend/Program/Package.hs` as the internal package/module owner.
- Define narrow types for package identity, module identity, and source units, for example `PackageId`, `PackageModuleId`, `ProgramSourceUnit`, `ProgramPackage`, and `LocatedProgramPackage`. Keep the types private to the internal library; do not export them from `src-public/MLF/API.hs` or `src-public/MLF/Pipeline.hs`.
- Provide trivial-package adapters for current parsed and located inputs, for example `trivialProgramPackage :: Program -> ProgramPackage` and `trivialLocatedProgramPackage :: LocatedProgram -> LocatedProgramPackage`.
- Provide owner-local flattening/projection helpers used only at the compatibility edge into the existing resolver/checker, for example `programPackageProgram :: ProgramPackage -> Program` and `locatedProgramPackageProgram :: LocatedProgramPackage -> LocatedProgram`. Name these as package projections, not as a second raw-program semantic mode.
- Provide package/source-unit combination helpers that let `MLF.Frontend.Program.Prelude.withPrelude` and `withPreludeLocated` prepend the built-in Prelude through the package owner instead of ad hoc module-list concatenation.

Expected checker/runtime routing:

- In `src/MLF/Frontend/Program/Check.hs`, add `checkProgramPackage` and `checkLocatedProgramPackage` as internal entrypoints. The existing `checkProgram` and `checkLocatedProgram` should delegate by first constructing trivial packages. Keep diagnostic span behavior for located inputs.
- Keep `resolveProgram` as the current resolver implementation for this round, but make the checker enter it only through the package owner's projection from a selected package. Do not add filesystem loading or cross-file discovery.
- In `src/MLF/Frontend/Program/Run.hs`, keep the public runtime functions unchanged. Runtime behavior may continue to call `checkProgram` / `checkLocatedProgram` as long as those functions now delegate through the package owner.
- In `src/MLF/Program/CLI.hs` and `src/MLF/Backend/Emission/Prepare.hs`, keep command signatures and public behavior unchanged, but use the package-owned trivial located package path when preparing parsed file inputs with the built-in Prelude.
- Update `mlf2.cabal` for any new module under `src/`.

Expected tests and docs:

- Add focused owner tests, preferably in a new `test/ProgramPackageSpec.hs` wired into `mlf2.cabal` and `test/Main.hs`, or in a clearly named `MLF.Program package owner` section if a separate spec is not needed.
- Cover at least:
  1. a located one-file program becomes a trivial package with a stable package id, one source unit, and module ids for its contained modules;
  2. package-owned checking of a trivial package gives the same main/module result as `checkProgram` for an existing single-file fixture;
  3. Prelude injection for a located trivial package still lets explicit `import Prelude` examples check or run;
  4. the current CLI helper and backend preparation behavior still work for existing single-file fixtures through the package-owned path.
- Update `docs/architecture.md` to name the new private package owner and state the current boundary precisely: existing one-file `.mlfp` inputs are trivial package source units; filesystem discovery, persistent interfaces, package roots, cache/build graph policy, and CLI package modes remain future milestones.

Do not widen `src-public/`, do not add compatibility shims for an old one-file semantic mode, do not create a duplicate resolver, and do not change roadmap files or controller state.

### Steps
1. Re-check current package-owner absence and entrypoints with:
   - `rg -n "Package|SourceUnit|trivialProgramPackage|checkProgramPackage|withPreludeLocated" src src-public app test docs/architecture.md mlf2.cabal`
   - `rg -n "checkProgram|checkLocatedProgram|runLocatedProgramOutput|prepareBackendEmissionFromSource|runProgramFile" src src-public app test`
2. Add `MLF.Frontend.Program.Package` with package identity, module identity, source-unit, trivial-package, located-trivial-package, projection, and package-combination helpers.
3. Refactor `MLF.Frontend.Program.Prelude.withPrelude` and `withPreludeLocated` to use the package owner for combining the built-in Prelude unit with the caller's trivial package input.
4. Add `checkProgramPackage` and `checkLocatedProgramPackage` in `MLF.Frontend.Program.Check`; route `checkProgram` and `checkLocatedProgram` through trivial-package construction before invoking the existing resolve/check implementation.
5. Confirm runtime entrypoints in `MLF.Frontend.Program.Run` now enter the owner through the checker path without changing public signatures or adding public package APIs.
6. Update `MLF.Program.CLI.runProgramFile` and `MLF.Backend.Emission.Prepare.prepareBackendEmissionFromSource` so parsed file inputs plus Prelude injection use the package-owned located-trivial-package path before running or checking.
7. Add focused owner tests and keep existing CLI/backend/public-surface expectations as real assertions, not smoke-only checks.
8. Register any new source or test modules in `mlf2.cabal`; if adding a new spec module, import and call it from `test/Main.hs`.
9. Update `docs/architecture.md` for the new owner boundary and explicitly record what this round does not implement.
10. Run focused validation, then the full behavior-changing gate.

### Verification
- `git diff --check`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI helper"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
- `cabal build all && cabal test`

Manual review checks:

- No public module under `src-public/` exports package-owner types or package-mode entrypoints in this milestone.
- The new owner proves package identity, module identity, source-unit shape, and trivial-package handling without filesystem discovery.
- Existing one-file `.mlfp` behavior is described and tested as trivial package input, not as a separate durable semantic mode.
- `docs/architecture.md` does not claim interface artifacts, package roots, search paths, cache policy, or package CLI modes are implemented.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They must conform to their schemas; `selection-record.json` remains the machine authority for selected lineage and scheduler fields.
