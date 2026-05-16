### Changes Made
- `src/MLF/Frontend/Program/Interface.hs`: added the private typed module/package interface artifact owner with checked exports, local data/class summaries, visible instances, source paths, direct `PackageModuleId` dependencies, fail-closed validation, and diagnostic rendering.
- `src/MLF/Frontend/Program/Check.hs`: routed prior-module import visibility through `ModuleInterface` accessors and validated package interfaces before returning successful checked packages.
- `test/ProgramInterfaceSpec.hs`: added focused interface artifact coverage for extraction, dependency metadata, import visibility, hidden constructor rejection, malformed artifact rejection, resolved identity ownership, and Prelude-owned builtin opaque exports.
- `test/PublicSurfaceSpec.hs`: added a guard that the private interface owner is not imported from public modules.
- `test/Main.hs`: wired `ProgramInterfaceSpec` into the Hspec harness.
- `mlf2.cabal`: registered `MLF.Frontend.Program.Interface` in the private internal library and `ProgramInterfaceSpec` in the test suite.
- `docs/architecture.md`: documented `MLF.Frontend.Program.Interface` as the private checked-summary owner and clarified that it is not a persisted ABI/cache or second typechecker authority.
- `docs/mlfp-resolved-symbol-identities.md`: documented the interface-specific exported-identity validation invariant.

### Tests
- `test/ProgramInterfaceSpec.hs`: verifies the new typed interface artifact contract, graph/source/dependency metadata, malformed artifact fail-closed behavior, and Prelude builtin export handling.
- `test/ProgramPackageDiscoverySpec.hs`: existing focused package filesystem discovery tests still pass through the interface-backed check path.
- `test/ResolvedSymbolSpec.hs` plus the new identity case in `ProgramInterfaceSpec`: resolved symbol identity behavior still passes, including interface export owner validation.
- `test/PublicSurfaceSpec.hs`: verifies public API surfaces remain unchanged and do not expose the private interface owner.

Verification run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program interface artifacts"'` passed: 6 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'` passed: 5 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program resolved symbol identities"'` passed: 5 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'` passed: 26 examples, 0 failures.
- `git diff --check` passed.
- `cabal build all && cabal test` passed: 2453 examples, 0 failures.

### Notes
The interface artifact is extracted only after module checking from `CheckedModule` facts plus `PackageModuleGraph`; it does not participate in type inference and does not repair failed checking. `Prelude` is the only module allowed to export builtin-owned opaque type identities such as `<builtin>.IO`; user modules still fail closed when exported identities do not define the exporting module.

An attempted parallel run of multiple focused `cabal test` commands raced on Cabal's in-place test package database and failed before executing two targets. Those targets were rerun sequentially and passed. The full gate rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` with the worktree absolute path; that generated churn was restored to the tracked root path before final diff checks.
