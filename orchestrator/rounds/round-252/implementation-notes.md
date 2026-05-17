### Changes Made
- `test/programs/compiler-seed/frontend-contract/SeedContract.mlfp`: added a minimal compiler frontend seed contract module as ordinary package-mode `.mlfp` source.
- `test/programs/compiler-seed/frontend-contract/Main.mlfp`: added a tiny pure `main` that imports and runs the seed contract through the interpreter path.
- `test/ProgramCompilerSeedSpec.hs`: added focused assertions for seed package discovery, module graph order, checking, interpreter output, and CLI `run-program` output.
- `test/Main.hs`: wired `ProgramCompilerSeedSpec` into the Hspec suite.
- `mlf2.cabal`: registered `ProgramCompilerSeedSpec` in the `mlf2-test` suite.
- `docs/mlfp-self-boot-readiness.md`: recorded the seed fixture owner, proof, and layer-separated non-goals without claiming self-hosting.
- `docs/architecture.md`: recorded the compiler seed fixture location and owner beside existing package-substrate evidence.
- `docs/mlfp-language-reference.md`: clarified that compiler-source seed fixtures use the same local package mode and do not imply ABI/linker/native/self-hosting support.

### Tests
- `test/ProgramCompilerSeedSpec.hs`: verifies the seed fixture is an ordinary package root, discovers `SeedContract` before `Main`, checks successfully, runs through `runLocatedProgramPackageOutput`, and produces the same result through CLI `run-program`.
- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`: PASS (2 examples, 0 failures).
- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS (2562 examples, 0 failures; 384.1171 seconds).
- `./scripts/thesis-conformance-gate.sh`: PASS (`[thesis-gate] PASS: thesis conformance anchors are green`).

### Notes
- A first space-containing matcher attempt (`--match=compiler frontend seed`) failed before test execution because Cabal split the Hspec matcher into separate arguments; the recorded focused validation uses the single-token `compiler-seed` matcher plus `--fail-on=empty`.
- The first real focused run found and fixed a fixture syntax issue: `.mlfp` case alternatives are semicolon-separated and do not allow a trailing semicolon before `}`.
- Native/backend behavior remains out of scope for this round; docs classify the seed as source-checking, package-discovery, interpreter-run, and CLI evidence only.
