### Changes Made
- `src/MLF/Platform/Contract.hs`: Extended `HostToolchainContract` with explicit target-scoped sysroot identity, system library identities, codegen settings, and linker mode declarations. Contract validation and deterministic rendering now include these fields, and `renderSubstrateFingerprintMaterial` keeps them in canonical fingerprint material.
- `src/MLF/Platform/ToolchainIdentity.hs`: Added the pure host toolchain identity validator over caller-provided declarations and observation snapshots. The owner defines `ObservedToolIdentity`, `ObservedToolchainSystemLibrary`, `ObservedToolchainIdentity`, `ToolchainIdentitySnapshot`, `ToolchainIdentityEvidence`, `ToolchainIdentityViolation`, `validatePlatformToolchainIdentity`, and deterministic evidence/violation renderers.
- `test/PlatformContractSpec.hs`: Updated the platform-contract substrate tests for expanded host toolchain rendering and fingerprint-material drift across sysroot identity, system library identity, codegen setting, and linker mode, alongside existing ABI, target, substrate component, tool identity, ambient-input, and loader-environment drift surfaces.
- `test/PlatformToolchainIdentitySpec.hs`: Added focused validation coverage for the host toolchain identity surface: deterministic evidence rendering, matched unavailable tools, target triple drift, missing and undeclared tool roles, tool path/digest/unavailable-reason/version drift, version-string-only identities, sysroot drift, system library drift, codegen setting drift, linker-mode drift, duplicate declaration/observation diagnostics, and pure repeatability over explicit snapshots.
- `test/golden/platform-contract/minimal-substrate-contract.txt`: Updated the canonical substrate contract golden to include sysroot identity, system library identities, codegen settings, and linker mode under the host toolchain section.
- `test/golden/platform-contract/host-toolchain-identity.txt`: Added the canonical valid host toolchain identity evidence fixture. It proves stable rendering for a matched explicit observation snapshot, sorted by tool role, system library name, and codegen key, covering target triple, resolved tools, sysroot, system libraries, codegen settings, and linker mode.
- `mlf2.cabal`: Registered `MLF.Platform.ToolchainIdentity` in the internal library and `PlatformToolchainIdentitySpec` in the test suite.
- `test/Main.hs`: Registered `PlatformToolchainIdentitySpec`.
- `docs/architecture.md`: Added `MLF.Platform.ToolchainIdentity` as the pure validation owner for declared host toolchain identity against explicit observation snapshots, with later-slice boundaries for real host discovery, checked package locks, native command/link/execution records, proof-manifest emission, and proof closeout.
- `orchestrator/rounds/round-360/implementation-notes.md`: Recorded implementer evidence for round-360. No `orchestrator/state.json` or active roadmap files were edited.

### Tests
- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`: PASS, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform toolchain identity"'`: PASS, 5 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders host toolchain identity evidence deterministically"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects toolchain identity drift with named diagnostics"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps toolchain identity validation pure over explicit snapshots"'`: PASS, 1 example, 0 failures.
- Static platform toolchain-identity guard after generated-artifact cleanup: PASS, output `round-360 platform toolchain-identity static guard passed`.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: PASS. The full `cabal test` run finished in 7444.3913 seconds with 2748 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS. Final output included `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
Validation stays pure over explicit `HostToolchainContract`, `TargetTriple`, and `ToolchainIdentitySnapshot` values. The new owner does not import or call host environment, directory, process, time, random, tool discovery, linker, or native-runner APIs.

The accepted golden path is `test/golden/platform-contract/host-toolchain-identity.txt`; it proves deterministic rendering for matched declared and observed toolchain identity, including resolved tools, sysroot identity, system library identities, codegen settings, and linker mode.

The full gate and thesis gate produced generated `runtime/mlfp_io/target/` noise. After capturing passing evidence, only generated target artifacts were restored or removed. Final `git status --short -- runtime/mlfp_io/target` was clean.

Explicit non-claims: this round does not implement lock validation, generated binding drift closure, host toolchain discovery, native command records, native link records, native execution records, package-manager/linker completion, platform/proof closeout, or self-boot completion. It is not milestone closeout.
