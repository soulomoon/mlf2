### Changes Made
- app/Main.hs: routes `check-program`, `run-program`, `emit-backend`, and `emit-native` to argument-list CLI helpers so each command accepts `<file-or-root> [--search-path <root> ...]`.
- src/MLF/Program/CLI.hs: added package-mode CLI argument parsing, file-vs-directory resolution, ordered local search-path discovery, `check-program`, and package-aware run/backend/native helpers while preserving file wrapper functions.
- src/MLF/Frontend/Program/Run.hs: added package-aware run/output adapters that check `ProgramPackage` / `LocatedProgramPackage` before runtime evaluation.
- src/MLF/Backend/Emission/Prepare.hs: added package-aware backend emission preparation adapters and kept source-string preparation as the trivial located-package path.
- src-public/MLF/Pipeline.hs: exposed narrow local package identity, source unit, discovery, check, and run APIs without exposing interface/build-graph/backend lowering internals.
- test/ProgramCliPackageSpec.hs: added focused CLI package migration coverage for file inputs, directory roots, ordered search paths, check/run/backend/native output, and CLI diagnostics.
- test/BackendEmissionPrepareSpec.hs: covered backend preparation from a located package.
- test/BackendLLVMSpec.hs: covered LLVM backend emission through the CLI package entrypoint.
- test/PublicSurfaceSpec.hs: covered public package discovery/check/run and guarded against CLI/interface/build-graph leakage.
- test/Main.hs and mlf2.cabal: wired the new spec module.
- README.md, docs/architecture.md, docs/mlfp-language-reference.md: documented local file/root/search-path package mode and kept package-manager, ABI, linker, and separate-compilation non-goals explicit.

### Tests
- test/ProgramCliPackageSpec.hs: verifies the approved CLI package migration surface.
- test/BackendEmissionPrepareSpec.hs: verifies backend emission prep accepts located packages.
- test/BackendLLVMSpec.hs: verifies CLI package input reaches LLVM emission.
- test/PublicSurfaceSpec.hs: verifies the narrow public package surface.
- Focused validation:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI package migration"'` — 6 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'` — 5 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package filesystem discovery"'` — 5 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package build graph policy"'` — 8 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'` — 3 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI package entrypoint"'` — 1 example, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM emits LLVM IR from the CLI file entrypoint"'` — 1 example, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'` — 27 examples, 0 failures.
- Full validation:
  - `git diff --check` — clean.
  - `cabal build all && cabal test` — build succeeded; 2470 examples, 0 failures.

### Notes
- The plan's combined `|` Hspec matchers selected 0 examples in this checkout, so the same gates were rerun as separate concrete matchers and passed.
- One parallel local Cabal invocation collided on `package.conf.inplace`; the affected focused matcher was rerun serially and passed.
- Support classification: source checking, interpreter/runtime, raw backend emission prep, and native LLVM CLI paths now accept local package mode. File inputs remain trivial packages. Directory roots and `--search-path` are local filesystem discovery only, with no package manager, remote dependency resolver, persisted interface format, stable `.mlfp` ABI, linker, or separate-compilation claim.
