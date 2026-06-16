### Changes Made

- Extended `src/MLF/Platform/Contract.hs` so ambient-input and loader-environment policies use typed rule records and dispositions (`scrubbed`, `declared`, `normalized`) instead of opaque string lists.
- Kept policy rules in deterministic contract rendering and substrate fingerprint material, including disposition and expected values.
- Added `src/MLF/Platform/EnvironmentPolicy.hs` as the pure owner for validating explicit ambient/loader snapshots and rendering deterministic evidence and violation diagnostics.
- Added `test/PlatformEnvironmentPolicySpec.hs` and wired it through `mlf2.cabal` and `test/Main.hs`.
- Updated `test/PlatformContractSpec.hs` and `test/golden/platform-contract/minimal-substrate-contract.txt` for typed policy rules and policy-rule fingerprint drift checks.
- Added `test/golden/platform-contract/normalized-environment-policy.txt`, proving stable evidence rendering for reordered ambient-input and loader-environment observations.
- Updated `docs/architecture.md` with the `MLF.Platform.EnvironmentPolicy` ownership boundary and future-slice non-claims.

### Tests

- `git diff --check`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`: passed, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform environment policy"'`: passed after tightening an initially over-specific spec assertion, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders normalized ambient and loader policy evidence deterministically"'`: passed, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects undeclared ambient and loader inputs with distinct diagnostics"'`: passed, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps environment policy validation pure over explicit snapshots"'`: passed, 1 example, 0 failures.
- Static platform environment-policy guard: passed with `round-359 platform environment-policy static guard passed`.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: passed, full suite ended at 2743 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes

- Policy validation remains pure over caller-provided `EnvironmentPolicySnapshot` values. It does not read host environment variables, spawn processes, query time/randomness, run linkers, or run native executables.
- Implemented violation coverage includes duplicate ambient rules, duplicate loader rules, undeclared observed ambient inputs, undeclared observed loader variables, scrubbed values observed as present, normalized value mismatches, blank names/variables, and blank normalized values.
- `test/golden/platform-contract/normalized-environment-policy.txt` is the canonical evidence fixture for a valid normalized ambient/loader snapshot and demonstrates stable sorting independent of input order.
- `test/PlatformContractSpec.hs` covers substrate fingerprint drift for ambient policy names, policy rule dispositions, normalized ambient values, and loader declared values.
- Full `cabal test` produced generated `runtime/mlfp_io/target/` noise; only those generated target artifacts were restored or removed after verification.
- Non-claims: this round does not implement lock validation, generated binding drift closure, host toolchain discovery, native link records, native execution records, package-manager/linker completion, platform/proof closeout, or self-boot completion.
