### Changes Made
- `src/MLF/Platform/PackageLock.hs`: added the pure checked local package lock owner with `SelfBootPackageLock`, locked/current package snapshot types, package lock evidence, drift/shape violations, deterministic lock/evidence rendering, and validation over caller-provided package/build metadata, ABI version, and substrate fingerprint material.
- `test/PlatformPackageLockSpec.hs`: added focused package-lock coverage for valid evidence, deterministic rendering under reordered package/module/dependency inputs, named drift diagnostics, duplicate and blank field diagnostics, and pure repeatability over explicit snapshots.
- `test/golden/platform-contract/checked-package-lock.txt`: added the deterministic checked local package lock evidence rendering for the valid compiler/prelude package closure.
- `mlf2.cabal`: registered `MLF.Platform.PackageLock` in the internal library and `PlatformPackageLockSpec` in the `mlf2-test` suite.
- `test/Main.hs`: wired `PlatformPackageLockSpec` into the test runner.
- `docs/architecture.md`: recorded `MLF.Platform.PackageLock` as the owner for pure checked local package lock validation and kept later-slice boundaries explicit.

### Tests
- `test/PlatformPackageLockSpec.hs`: verifies accepted checked lock evidence, sorted deterministic rendering, ABI version drift, substrate fingerprint drift, normalized local root drift, source metadata drift, direct dependency id drift, dependency interface metadata drift, interface metadata drift, missing/extra package diagnostics, missing/extra module diagnostics, duplicate package/module/dependency-interface diagnostics, blank/missing package identity/root diagnostics, and repeatability without host probing.
- `test/golden/platform-contract/checked-package-lock.txt`: proves the stable evidence text emitted for a valid checked local package lock; reordered lock and current snapshot inputs render identically.
- `git diff --check`: PASS.
- Static platform package-lock guard: PASS with output `round-361 platform package-lock static guard passed`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`: PASS, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform package lock"'`: PASS, 4 examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders checked local package lock evidence deterministically"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects checked package lock drift with named diagnostics"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps package lock validation pure over explicit snapshots"'`: PASS, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: PASS. The recovered implementer reran this full gate because no durable full-gate log was present after transport recovery; the run ended with `2752 examples, 0 failures` and `Test suite mlf2-test: PASS`.
- `./scripts/thesis-conformance-gate.sh`: PASS with final output `PASS: thesis conformance anchors are green`.

### Notes
- Cleanup: full Cabal/thesis runs regenerated `runtime/mlfp_io/target/` artifacts (`.rustc_info.json`, release library metadata, cargo locks/fingerprints, and release deps). Only those generated artifacts were restored/removed after evidence capture; final `git status --short -- runtime/mlfp_io/target` is clean.
- This slice owns pure validation over explicit package-lock snapshots only. It does not discover package roots, hash sources, regenerate locks, parse a final `.mlfp` package-lock format, solve packages, run native linkers, emit command records, or integrate proof runners.
- Non-claims: no generated binding drift closure, package-manager completion, lockfile parser completion, native command records, native link records, native execution records, platform/proof closeout, M5 closeout, self-boot completion, or full self-boot claim.
