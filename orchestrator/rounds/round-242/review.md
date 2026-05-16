### Checks Run
- Command: `git status --short --branch`
  Result: pass; branch is `orchestrator/round-242-cli-emission-preparation` in `/Volumes/src/mlf4/orchestrator/worktrees/round-242`.
- Command: `git diff --check`
  Result: pass; no whitespace errors.
- Command: `cabal test mlf2-test --test-options "--match BackendEmissionPrepareSpec"`
  Result: pass; 2 examples, 0 failures.
- Command: `cabal test mlf2-test --test-option=--match --test-option="emits LLVM IR from the CLI file entrypoint"`
  Result: pass; equivalent matcher form for the plan's CLI file-entrypoint selector, 1 example, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass; full suite passed with 2583 examples, 0 failures.
- Command: `git diff --name-only -- src-public`
  Result: pass; no public API files changed.

### Plan Compliance
- Add private backend adapter: met. `src/MLF/Backend/Emission/Prepare.hs` defines `BackendEmissionPreparationError`, `renderBackendEmissionPreparationError`, `prepareBackendEmissionFromSource`, and `prepareCheckedProgramForBackendEmission`.
- Move backend-emission helper logic out of CLI: met. Prelude pruning, binding/data dependency closures, source-type traversal, and free `ElabTerm` variable traversal now live in `MLF.Backend.Emission.Prepare`; `MLF.Program.CLI` no longer imports `Data.Map.Strict`, `Data.Set`, `MLF.Elab.Types`, or low-level checked-program types for backend preparation.
- Keep CLI as file/command adapter: met. `emitBackendFile` and `emitNativeFile` read files, delegate to `prepareBackendEmissionFromSource`, render through `MLF.Backend.LLVM`, and present errors; `runProgramFile` remains in `MLF.Program.CLI`.
- Register modules: met. `MLF.Backend.Emission.Prepare` is registered in the private internal library, and `BackendEmissionPrepareSpec` is registered in `mlf2.cabal` and `test/Main.hs`.
- Add focused coverage: met. `BackendEmissionPrepareSpec` exercises source-string preparation/rendering without temp-file IO and Prelude retention/pruning behavior; the existing CLI file-entrypoint selector still passes.
- Update architecture docs: met. `docs/architecture.md` records `MLF.Backend.Emission.Prepare` as backend-emission semantic preparation owner and `MLF.Program.CLI` as command/file owner.
- Check stale duplicate ownership: met. No `src-public/` changes, no public lowering API, no duplicate backend IR surface, and no backend-specific Prelude-pruning helpers remain in `MLF.Program.CLI`.

### Decision
**APPROVED**

### Evidence
The diff satisfies `milestone-4` / `direction-4a-cli-emission-preparation` / `item-4a-backend-emission-preparation-adapter`: semantic backend-emission preparation moved to the private backend adapter, while CLI retains file IO, command dispatch, LLVM render dispatch, and user-facing error rendering. The milestone-4 completion signal is satisfied by this round, so closeout is status-only: mark `milestone-4` from `pending` to `done` and add compact completion/history pointers through `roadmap-view.json` anchors.
