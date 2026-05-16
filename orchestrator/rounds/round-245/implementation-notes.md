### Changes Made
- `src/MLF/Frontend/Program/Package.hs`: added the private `.mlfp` package/module owner with package ids, module ids, source units, trivial-package adapters, package projections, and source-unit prepend helpers.
- `src/MLF/Frontend/Program/Check.hs`: added `checkProgramPackage` and `checkLocatedProgramPackage`; existing program checkers now enter through trivial package construction before projecting to the current resolver/checker implementation.
- `src/MLF/Frontend/Program/Prelude.hs`: routed built-in Prelude injection through package source-unit combination helpers while preserving existing `withPrelude` and `withPreludeLocated` signatures.
- `src/MLF/Program/CLI.hs`: changed `runProgramFile` to build a located trivial package, prepend the Prelude through the package owner, and project back for the unchanged runtime API.
- `src/MLF/Backend/Emission/Prepare.hs`: changed source-string backend preparation to check the Prelude-prepended located trivial package via `checkLocatedProgramPackage`.
- `mlf2.cabal`: registered `MLF.Frontend.Program.Package` and `ProgramPackageSpec`.
- `test/Main.hs`: wired `ProgramPackageSpec` into the Hspec suite.
- `docs/architecture.md`: recorded `MLF.Frontend.Program.Package` as the private package/module owner and clarified that one-file `.mlfp` inputs are trivial package source units; filesystem discovery, interfaces, package roots/search paths, cache/build graph policy, stable ABI/linking, and CLI package mode remain future milestones.

### Tests
- `test/ProgramPackageSpec.hs`: verifies trivial located package identity/source-unit/module ids, direct package checking parity with `checkProgram`, Prelude source-unit injection, CLI single-file behavior, and backend preparation through the package-owned located path.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program package owner"'`: passed, 5 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI helper"'`: passed, 30 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BackendEmissionPrepareSpec"'`: passed, 2 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`: passed, 25 examples.
- `git diff --check`: passed.
- `cabal build all && cabal test`: passed; full Hspec suite reported 2440 examples, 0 failures.

### Notes
No `src-public/` modules, roadmap coordination files, or `orchestrator/state.json` were changed. The full Cabal gate emitted existing warnings from untouched modules during the build, but the package-owner changes compiled and tested successfully.
