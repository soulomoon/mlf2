### Changes Made
- `src/MLF/Platform/Contract.hs`: Added the bounded platform substrate contract owner for `item-358-platform-substrate-contract-fingerprint-material`. The module owns a pure typed declaration model, validation errors, deterministic line-oriented contract rendering, and canonical fingerprint-material rendering for ABI version, contract package identity, target triple, trusted substrate component identities, host toolchain identities, ambient-input policy, and loader-environment policy.
- `test/PlatformContractSpec.hs`: Added focused platform contract substrate coverage for golden rendering, ordering stability, fingerprint-material drift, named validation diagnostics, and pure repeatability over explicit declarations.
- `test/golden/platform-contract/minimal-substrate-contract.txt`: Added the canonical minimal substrate contract rendering fixture. It proves the declaration render is stable, line-oriented, sorted by stable component/tool keys, and includes ABI, package identity, target triple, substrate component identities, host toolchain identity fields, ambient-input policy, and loader-environment policy.
- `mlf2.cabal`: Wired `MLF.Platform.Contract` into the private internal library surface and wired `PlatformContractSpec` into the test suite.
- `test/Main.hs`: Registered `PlatformContractSpec` with the focused test runner.
- `docs/architecture.md`: Added the `MLF.Platform.Contract` ownership entry and scoped it to declaration/fingerprint-material substrate identity.
- `orchestrator/rounds/round-358/implementation-notes.md`: Recorded the implementer evidence, guard output, verification results, cleanup note, and non-claims in the implementer role structure.

### Tests
- `test/PlatformContractSpec.hs`: Verifies canonical rendering against `test/golden/platform-contract/minimal-substrate-contract.txt`; verifies equivalent component/tool reordering keeps canonical fingerprint material stable; verifies changes to ABI version, target triple, substrate component digest, host toolchain identity, ambient-input policy, and loader-environment policy change the canonical fingerprint material; verifies invalid declarations render diagnostics naming the failing contract surface; verifies rendering is pure over explicit declarations without environment, wall-clock, random, linker, native execution, or host path probing in the owner/spec.
- `test/golden/platform-contract/minimal-substrate-contract.txt`: Golden evidence for the minimal valid substrate contract declaration render.
- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`: PASS, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders deterministic substrate contract declarations"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "changes substrate fingerprint material when declared platform identity changes"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects invalid platform substrate contract declarations with named diagnostics"'`: PASS, 1 example, 0 failures.
- Static platform-contract guard: PASS, output `round-358 platform contract static guard passed`.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: PASS. Full `cabal test` result was 2739 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS. Final output included `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
`MLF.Platform.Contract` validates missing or empty ABI/package/target fields, duplicate substrate component keys, duplicate host toolchain roles, host toolchain entries that only provide a version string without resolved identity fields, missing ambient-input policy, and missing loader-environment policy.

Rendering is owner-local, deterministic, line-oriented, and sorted by substrate component key and host toolchain role. `renderSubstrateFingerprintMaterial` returns canonical fingerprint material only; it does not compute a final digest.

The full Cabal gate generated `runtime/mlfp_io/target/` noise; those generated artifacts were restored or removed after verification.

This round does not claim final cryptographic substrate digest, lock validation, generated binding drift closure, native link records, native execution records, package manager/linker completion, platform/proof closeout, or self-boot completion.
