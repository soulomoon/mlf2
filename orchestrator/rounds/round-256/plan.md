### Selected Extraction
- Milestone: Runtime And Backend Layer Classification
- Milestone id: `milestone-5`
- Direction id: `direction-5a-interpreter-native-boundary-ledger`
- Extracted item id: `item-256-runtime-backend-layer-classification`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Classify the merged compiler frontend seed across source checking, interpreter/runtime, backend/native, object-code, and package-build layers. The round should preserve the interpreter-first boundary, document or test explicit backend/native fail-closed behavior, and avoid changing the seed contract or forcing native lowerability.

### Approach
Keep the extraction serial and inside `milestone-5`. Milestones 3 and 4 are done, so the parser seed and primitive/stdlib budget are available. Milestone 6 remains blocked until this layer classification is coherent.

Start from the compiler seed package at `test/programs/compiler-seed/frontend-contract/` and the round-255 budget in `docs/mlfp-self-boot-readiness.md`. Classify each seed module and the package as a whole using current repo evidence:

- source checking: package discovery, module graph, and `check-program`;
- interpreter/runtime: `runLocatedProgramPackageOutput` and CLI `run-program` evidence;
- backend/native: `emit-backend` and `emit-native` package entrypoints, backend emission preparation, LLVM/native diagnostics, and `docs/backend-native-pipeline.md`;
- object code: current native/object parity policy and tool-gated coverage, without adding a stable object ABI claim;
- package build mode: local package-root and search-path behavior, not a package manager or separate compilation mode.

If the seed is intentionally interpreter-only for backend/native or object-code purposes, record that as an explicit layer classification and pin the fail-closed diagnostic only where a focused, non-brittle test can do so. Do not add a second public backend IR, lazy runtime/STG machinery, broad FFI lane, native runtime redesign, or new primitive/Prelude surface.

### Steps
1. Inspect the current seed and its evidence: `test/programs/compiler-seed/frontend-contract/*.mlfp`, `test/ProgramCompilerSeedSpec.hs`, `orchestrator/rounds/round-254/implementation-notes.md`, and `orchestrator/rounds/round-255/implementation-notes.md`.
2. Inspect layer owners before editing: `src/MLF/Program/CLI.hs`, `src/MLF/Backend/Emission/Prepare.hs`, `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM.hs`, `test/ProgramCliPackageSpec.hs`, `test/ProgramFixturePackageSpec.hs`, `test/BackendLLVMSpec.hs`, `test/Parity/ProgramMatrix/NativePolicy.hs`, `docs/backend-native-pipeline.md`, and `docs/mlfp-self-boot-readiness.md`.
3. Add the narrowest durable classification artifact, likely a new or updated section in `docs/mlfp-self-boot-readiness.md`, that lists every compiler seed module plus the package-level entrypoint and records its status for source checking, interpreter/runtime, backend-lowerable, native-runnable, object-code, and package-build layers.
4. Run the current package entrypoints against the seed before choosing test scope: `check-program`, `run-program`, `emit-backend`, and `emit-native` on `test/programs/compiler-seed/frontend-contract`. Use the observed results to classify backend/native support as supported, unsupported with diagnostic, or out of scope for this family.
5. Add focused tests only for behavior that is important and stable enough to guard. Preferred locations are the existing compiler-seed/package CLI specs rather than a new broad suite. Good candidates are exact source/interpreter evidence already asserted and one explicit backend/native unsupported diagnostic if the seed crosses an unsupported shape. Avoid smoke-only tests and avoid prose substring guards for the ledger.
6. Update docs without overclaiming. Source checking and interpreter success may be described as proved by the seed; backend/native and object-code claims must point to actual lowerability evidence or explicit fail-closed diagnostics. Package-build wording must stay local package-mode only, with no package manager, stable ABI, linker, persisted interface format, or separate object build claim.
7. Review scope before closeout: no seed grammar changes, no parser result contract changes, no primitive/stdlib expansion, no checker/backend implementation in `.mlfp`, no public API widening under `src-public/`, no second backend IR, and no roadmap-file changes.

### Verification
Run the focused seed and layer checks first. For `emit-backend` and `emit-native`, the acceptable outcome is either successful lowerability evidence or an explicit fail-closed diagnostic that is recorded in the classification; do not treat unsupported native/backend behavior as a reason to widen scope.

```sh
cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'
cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- emit-backend test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- emit-native test/programs/compiler-seed/frontend-contract
```

If focused backend/native assertions are added under existing specs, run the matching spec slice as well, for example:

```sh
cabal test mlf2-test --test-options='--match=package' --test-options='--fail-on=empty'
cabal test mlf2-test --test-options='--match=backend' --test-options='--fail-on=empty'
```

Then run the required hygiene and behavior-changing gates:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review must confirm every seed module and package layer is classified, unsupported backend/native cases fail closed or are explicitly out of scope, docs stay layer-separated, and no unsupported layer is converted into an implementation claim.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
