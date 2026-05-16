### Checks Run
- Command: `git status --short --branch`
  Result: pass. Worktree is on `orchestrator/round-249-cli-api-package-migration`; diff is scoped to CLI/package migration code, public package API surface, docs, Cabal/test wiring, and round artifacts. No `orchestrator/state.json` edit is present.
- Command: `git diff --check`
  Result: pass. No whitespace errors after restoring generated Rust depfile churn from the full gate.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI package migration"'`
  Result: pass. 6 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner|MLF.Program package filesystem discovery|MLF.Program package build graph policy"'`
  Result: informational only. This Hspec matcher selected 0 examples in this checkout, so the intended coverage was run with the separate concrete matchers below.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'`
  Result: pass. 5 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'`
  Result: pass. 5 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package build graph policy"'`
  Result: pass. 8 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`
  Result: pass. 3 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI file entrypoint|package"'`
  Result: informational only. This Hspec matcher selected 0 examples in this checkout, so the intended coverage was run with the separate concrete matchers below.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI package entrypoint"'`
  Result: pass. 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI file entrypoint"'`
  Result: pass. 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
  Result: pass. 27 examples, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass. Full test suite passed with 2470 examples, 0 failures.
- Command: manual diff review of `app/Main.hs`, `src/MLF/Program/CLI.hs`, `src/MLF/Frontend/Program/Run.hs`, `src/MLF/Backend/Emission/Prepare.hs`, `src-public/MLF/Pipeline.hs`, `test/ProgramCliPackageSpec.hs`, backend/public-surface tests, Cabal/test wiring, `README.md`, `docs/architecture.md`, and `docs/mlfp-language-reference.md`.
  Result: pass. The implementation matches the round plan, implementation notes, and milestone-5 verification contract.

### Plan Compliance
- Step 1: met. The relevant CLI, package, checker/runtime, backend preparation, public surface, tests, docs, and Cabal wiring were reviewed against the plan and implementation notes.
- Step 2: met. `test/ProgramCliPackageSpec.hs` adds focused coverage under `MLF.Program CLI package migration` and is wired into `mlf2.cabal` and `test/Main.hs`.
- Step 3: met. Tests cover file input as a Prelude-prepended trivial package, directory root discovery, ordered `--search-path` roots, malformed option handling, and missing file-or-root diagnostics naming the requested path.
- Step 4: met. `check-program` is covered for both an existing file input and a multi-file directory/search-path package, returning stable `OK` output on success.
- Step 5: met. `run-program` is covered for an existing single-file fixture and for a multi-file package where `Main` imports `Lib` from a search-path root.
- Step 6: met. `emit-backend` is covered on a multi-file package, asserting both `Lib__two` and `Main__main` appear in valid LLVM.
- Step 7: met. `emit-native` package mode is covered at the LLVM boundary, including the native process entrypoint and LLVM validation.
- Step 8: met. `MLF.Frontend.Program.Run` adds package-aware run/output adapters implemented through `checkProgramPackage` and `checkLocatedProgramPackage`.
- Step 9: met. `MLF.Backend.Emission.Prepare` adds package-aware preparation for `ProgramPackage` and `LocatedProgramPackage`; source-string preparation now delegates through the located trivial package path.
- Step 10: met. `MLF.Program.CLI` owns argument parsing, file-vs-directory inspection, local search-path discovery, Prelude injection, command rendering, and user-facing error text while delegating semantic checking/runtime/backend preparation to package/backend owners.
- Step 11: met. `app/Main.hs` routes `check-program`, `run-program`, `emit-backend`, and `emit-native` through the package-aware argument-list helpers.
- Step 12: met. `MLF.Pipeline` exposes narrow package identity/root/search-path, source-unit, discovery, check, and run functions. It does not expose private interface/build-graph/cache modules, backend lowering helpers, or LLVM rendering APIs.
- Step 13: met. `PublicSurfaceSpec` proves public package discovery/check/run on a multi-file package and guards against public imports of `MLF.Frontend.Program.Interface`, `MLF.Frontend.Program.BuildGraph`, and `MLF.Program.CLI`.
- Step 14: met. README, architecture, and language reference docs describe file/root/search-path package mode and explicitly keep package manager, remote dependency, persisted interface, stable ABI, linker, separate compilation, fixture migration, and self-boot claims out of scope.
- Step 15: met. Existing one-file entrypoints remain only as trivial package adapters; user-facing CLI commands and new public package adapters go through package-owned checking/preparation rather than a separate durable one-file semantic path.

### Decision
**APPROVED**

### Evidence
The integrated round satisfies milestone-5's completion signal. `run-program`, `emit-backend`, `emit-native`, and the new `check-program` command all resolve file-or-root inputs into `LocatedProgramPackage`s, inject the Prelude as a package source unit, and route through package-owned checking/runtime/backend preparation. File inputs remain supported as trivial packages; directory roots and ordered `--search-path` roots are exercised by focused tests.

The public API migration stays narrow: `MLF.Pipeline` exposes local package identity/source-unit/discovery/check/run types and functions, while private interface/build graph/cache details and backend lowering/LLVM rendering remain internal. Support is correctly layered: source checking, interpreter/runtime, raw backend emission, and native LLVM emission accept local package mode; object-code/native-linked package execution is not newly claimed; package build mode remains local filesystem discovery only.

Focused task-specific tests, public-surface tests, backend preparation coverage, diff hygiene, and the full Cabal gate all passed. The full gate rewrote the generated Rust depfile path to the worktree absolute path during validation; that generated churn was restored before this review artifact was written.

Closeout classification: status-only. The implementation completes the existing milestone-5 contract without changing future coordination, milestone meaning, sequencing, extraction scope, verification policy, retry policy, or compatibility posture. The controller can apply `milestone-5` status `pending -> done` through `milestone-5-status` and add the compact completion pointer through `milestone-5-completion`.
