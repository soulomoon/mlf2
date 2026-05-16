### Selected Extraction
- Milestone: CLI Emission Preparation
- Milestone id: `milestone-4`
- Direction id: `direction-4a-cli-emission-preparation`
- Extracted item id: `item-4a-backend-emission-preparation-adapter`
- Roadmap id: `2026-05-16-00-architecture-deepening-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001`

### Goal
Move the pure backend-emission preparation path out of `MLF.Program.CLI` and into a private backend-owned adapter. After this round, `MLF.Program.CLI` should still own command selection, user-facing file IO, and error presentation, while backend emission owns parsing a provided source string for backend emission, Prelude injection, checking, Prelude-retention pruning, and the checked-program artifact handed to `MLF.Backend.LLVM`.

### Approach
Create a private backend adapter module, proposed as `MLF.Backend.Emission.Prepare`, under `src/MLF/Backend/Emission/Prepare.hs`. The adapter should expose a small internal surface:

- `BackendEmissionPreparationError(..)`
- `renderBackendEmissionPreparationError`
- `prepareBackendEmissionFromSource :: FilePath -> String -> Either BackendEmissionPreparationError CheckedProgram`
- `prepareCheckedProgramForBackendEmission :: CheckedProgram -> CheckedProgram`

`prepareBackendEmissionFromSource` should be pure with respect to file IO: it receives the caller's display path and source text, parses with `parseLocatedProgramWithFile`, applies `withPreludeLocated`, checks with `checkLocatedProgram`, and then applies `prepareCheckedProgramForBackendEmission`.

Move the existing Prelude-retention policy from `MLF.Program.CLI` into this backend adapter, including the dependency closure over referenced Prelude bindings, referenced Prelude data, constructor-owned data, source-type heads, and free elaborated term variables. Keep this adapter inside the private `mlf2-internal` library; do not add any module or helper under `src-public/`, and do not add a public lowering API solely for tests.

`MLF.Program.CLI` should become a thin file/command adapter for the emission commands: read the requested file, pass the path and source text to the backend preparation adapter, then call `renderCheckedProgramLLVM` or `renderCheckedProgramNativeLLVM`. Leave `runProgramFile` in `MLF.Program.CLI`; runtime execution is not this round's backend-emission extraction.

Use serial implementation only. The selected files share imports and execution paths, and worker fan-out is not justified for this slice.

### Steps
1. Add `src/MLF/Backend/Emission/Prepare.hs` with the adapter API above. Define `BackendEmissionPreparationError` constructors for program parse and program diagnostic failures, and render them with the existing frontend renderers.
2. Move the current backend-emission helper logic out of `src/MLF/Program/CLI.hs`: `emitBackendCheckedProgram`, Prelude module filtering, binding/data dependency closures, source-type traversal, and free `ElabTerm` variable traversal should live in the new backend adapter.
3. Update `src/MLF/Program/CLI.hs` so `emitBackendFile` and `emitNativeFile` only perform file reading, delegate semantic preparation to `prepareBackendEmissionFromSource`, and render through `MLF.Backend.LLVM`. The module should no longer import `Data.Map.Strict`, `Data.Set`, `MLF.Elab.Types`, or low-level `MLF.Frontend.Program.Types` solely for backend-emission preparation.
4. Register the new adapter module in the private internal library stanza in `mlf2.cabal`. Keep it out of `src-public/` and out of public facade exports.
5. Add focused coverage for the new seam. Prefer a new `test/BackendEmissionPrepareSpec.hs` wired into both `mlf2.cabal` and `test/Main.hs`. It should verify a source string can be prepared and rendered through LLVM without creating a temp file when file IO is not the behavior under test, while preserving an existing CLI file-entrypoint assertion for command/file wiring.
6. Update `docs/architecture.md` to record that `MLF.Backend.Emission.Prepare` owns backend-emission semantic preparation and that `MLF.Program.CLI` owns command/file orchestration. Do not claim milestone completion beyond this first extracted adapter slice.
7. Check for stale duplicate ownership after the move. `MLF.Program.CLI` should not retain backend-specific Prelude pruning helpers, and backend tests should not need to drive temp files merely to exercise preparation behavior.

### Verification
Run focused checks first:

```sh
git diff --check
cabal test mlf2-test --test-options "--match BackendEmissionPrepareSpec"
cabal test mlf2-test --test-options "--match emits LLVM IR from the CLI file entrypoint"
```

Before review, because this changes behavior-adjacent backend/CLI code, run the full gate:

```sh
cabal build all && cabal test
```

Review should also manually confirm:

- `selection-record.json` and `round-plan-record.json` still name `milestone-4`, `direction-4a-cli-emission-preparation`, and `item-4a-backend-emission-preparation-adapter`.
- No public modules under `src-public/` changed.
- No public lowering API or duplicate backend IR was introduced.
- `MLF.Program.CLI` still owns command selection and file IO, but no longer owns backend-emission preparation policy.
- `docs/architecture.md` describes the new ownership boundary without overstating completion of later milestone-4 cleanup.
