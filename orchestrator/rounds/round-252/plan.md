### Selected Extraction
- Milestone: Compiler Source Package And Seed Contract
- Milestone id: `milestone-1`
- Direction id: `direction-1a-compiler-source-package-contract`
- Extracted item id: `item-252-compiler-source-package-contract`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Create the first ordinary package-mode home and executable proof contract for compiler frontend seed source modules. This round should add a tiny `.mlfp` compiler-seed package fixture and focused assertions that the package is discovered, checked, and run through the current interpreter path, then update readiness/architecture docs to name the seed owner, fixture policy, and non-goals.

### Approach
Keep the extraction serial and inside `milestone-1`. Reuse `MLF.Frontend.Program.Package`, `MLF.Frontend.Program.Check`, `MLF.Frontend.Program.Run`, `MLF.Program.CLI`, and the existing package fixture/test conventions. Do not add a compiler-source loader, one-file compatibility path, checker/parser/backend scope, native requirement, package manager, ABI, linker, or public facade unless the existing package-mode entrypoints prove a narrow test-support need.

The expected implementation shape is:
- an intentional static fixture root under `test/programs/` for compiler frontend seed source modules;
- ordinary `.mlfp` modules with explicit imports/exports and a tiny pure `main` suitable for interpreter execution;
- a focused Hspec module or focused extension of the existing package fixture specs that asserts discovery, module graph/order where relevant, check success, interpreter output, and CLI `run-program` behavior for that fixture;
- docs updates in `docs/mlfp-self-boot-readiness.md` and, if ownership wording changes, `docs/architecture.md` / `docs/mlfp-language-reference.md`, without claiming lexer/parser/checker/self-hosting progress.

### Steps
1. Inspect the live package-mode entrypoints and fixtures before editing: `src/MLF/Program/CLI.hs`, `src/MLF/Frontend/Program/Package.hs`, `src/MLF/Frontend/Program/Check.hs`, `src/MLF/Frontend/Program/Run.hs`, `test/ProgramCliPackageSpec.hs`, `test/ProgramFixturePackageSpec.hs`, `test/ProgramPackageDiscoverySpec.hs`, `test/Parity/ProgramMatrix.hs`, `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, and `docs/architecture.md`.
2. Add the smallest compiler-seed package fixture under an intentional package-mode location such as `test/programs/compiler-seed/frontend-contract/`. Use ordinary module names accepted by the current parser, keep the source pure and interpreter-runnable, and make the module names/content communicate compiler-frontend seed ownership without implementing token, lexer, parser, checker, backend, package-manager, ABI, linker, or driver behavior.
3. Add focused package-mode assertions. Prefer a new `ProgramCompilerSeedSpec` when that keeps the seed contract discoverable; if a new spec module is added, register it in both `mlf2.cabal` and `test/Main.hs`. At minimum assert that the fixture is discovered as a package root, checks successfully, runs through `runLocatedProgramPackageOutput` or `runProgramArgs`, and returns the exact expected value.
4. Add a CLI-level assertion for `run-program <seed-root>` so the public executable path proves the seed package uses the same package-mode interpreter entrypoint as other `.mlfp` packages.
5. Update readiness and ownership docs to say that a compiler frontend seed package contract exists, where the fixture lives, what it proves, and what it still does not prove. Keep all support claims layer-separated and leave lexer/parser/checker/backend/native/object-code/package-build gaps explicit.
6. Verify there is no duplicate loader, no new one-file semantic mode, no public API widening without direct evidence, no stale unregistered test module, and no docs overclaim beyond the runnable seed package smoke.

### Verification
Run focused validation first:

```sh
cabal test mlf2-test --test-options '--match "/compiler frontend seed|compiler seed|package migration|fixture package/"'
```

Adjust the matcher to the exact spec names created or touched. Then run:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review should confirm the selected fixture root is an ordinary local package root, the tests assert concrete interpreter output rather than smoke-only execution, docs do not claim self-hosting or lexer/parser implementation, and unsupported native/backend behavior remains out of scope for this round.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
