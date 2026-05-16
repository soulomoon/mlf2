### Selected Extraction
- Milestone: Native Parity Policy Module
- Milestone id: milestone-3
- Direction id: direction-3a-native-parity-policy-module
- Extracted item id: item-3a-program-matrix-native-policy-owner
- Roadmap id: 2026-05-16-00-architecture-deepening-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001

### Goal

Create one test-support owner for backend/native/object-code parity policy beside `Parity.ProgramMatrix`, then make `BackendLLVMSpec` consume that owner instead of keeping local manual string lists for object-code and native-run coverage. The extracted policy must classify each interpreter-success row by layer: source checking, interpreter/runtime, backend LLVM assembly, object-code smoke, native executable run, and tool availability.

### Approach

`milestone-3` is the first lawful dependency-ready unfinished milestone at HEAD: `roadmap-view.json` marks milestones 1 and 2 done, marks `milestone-3` pending, and later milestones depend on it. Current repo evidence shows the ownership gap is live:

- `test/Parity/ProgramMatrix.hs` owns the interpreter-success row set through `programSpecToLLVMParityCases` and `programRuntimeSuccessCases`.
- `test/ProgramSpec.hs` consumes `programRuntimeSuccessCases` for interpreter/runtime assertions and rejects runtime-success rows in its non-runtime matrix checks.
- `test/BackendLLVMSpec.hs` consumes `programSpecToLLVMParityCases`, but keeps backend/object/native coverage policy locally through `llvmParityCoverage`, `llvmObjectCodeParityCases`, `llvmRequiredNativeRunParityCases`, `backendFirstOrderSurfaceParityNames`, and `backendUnifiedFixturePaths`.
- `test/LLVMToolSupport.hs` owns tool discovery and native execution mechanics, but it does not own row-level support policy.
- `test/Parity/FrozenArtifacts.hs` is a separate frozen baseline helper; it should be audited for policy coupling but not pulled into native/object-code policy unless the implementation finds a real shared row-selection dependency.

Keep this round serial. The intended implementation is a test-support module such as `Parity.NativePolicy` or `Parity.ProgramMatrix.NativePolicy` under `test/Parity/`, registered in `mlf2.cabal`, with backend parity tests rewired to import it. Do not widen `src-public/`, do not introduce production native policy APIs, and do not convert real native/object assertions into skip-only smoke checks.

### Steps

1. Audit the live parity authorities in `test/Parity/ProgramMatrix.hs`, `test/ProgramSpec.hs`, `test/BackendLLVMSpec.hs`, `test/LLVMToolSupport.hs`, `test/Parity/FrozenArtifacts.hs`, and `docs/backend-native-pipeline.md`. Confirm the canonical interpreter-success input set remains `programSpecToLLVMParityCases`.
2. Add a test-support parity policy owner beside `Parity.ProgramMatrix`. The owner should expose a concrete row policy type with the runtime case, case name, source, expected interpreter/runtime result, source-checking status, backend LLVM assembly expectation, object-code expectation, native-run expectation, and LLVM/native toolchain requirements.
3. Move the manual backend parity string-list authority out of `BackendLLVMSpec`: at minimum, object-code smoke membership and required native-run membership should become data exported by the new policy owner and derived or checked against `programSpecToLLVMParityCases`. If the current matrix has no native-unsupported exceptions, encode that explicitly as policy rather than leaving unsupported cases implicit.
4. Rewire the `ProgramSpec-to-LLVM parity matrix` block in `BackendLLVMSpec` to iterate policy rows from the owner. Raw LLVM assembly validation, optional object-code validation, native LLVM emission, native object-code validation, native executable run, and stdout/stderr/exit-code comparison should be selected from the policy record, not from local name lists.
5. Preserve real assertions. The current backend parity path native-runs interpreter-success rows and compares `ProgramRuntimeExpectation`; the implementation must keep that strength for rows classified as native-run supported. If a row is classified unsupported, the test must assert the stable unsupported diagnostic at the backend/native boundary instead of silently skipping it.
6. Add or tighten guard coverage so every `programSpecToLLVMParityCases` row has exactly one policy entry, every policy entry names a real runtime row, object-code/native-run lists contain no stale names, and the layer classification is visible in failure messages. Keep `ProgramSpec` interpreter assertions on the runtime matrix; do not make interpreter tests depend on backend policy.
7. Update `docs/backend-native-pipeline.md` and, if module ownership wording changes, `docs/architecture.md` to name the parity policy owner and the layer split. The docs must distinguish source checking, interpreter/runtime, backend LLVM assembly, object-code, native executable behavior, and tool availability without claiming frozen parity helpers own native policy.
8. Keep frozen parity helper changes minimal. If `Parity.FrozenArtifacts` remains unrelated to native/object-code row selection, record that as a no-change justification in implementation notes rather than inventing a dependency.

### Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix/classifies every interpreter-success case exactly once"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
- Manual audit: `BackendLLVMSpec` no longer owns standalone manual string-list policy for ProgramSpec-to-LLVM object-code/native parity; it consumes the test-support owner instead.
- Manual audit: every row is classified by source checking, interpreter/runtime, backend LLVM assembly, object-code, native run, and tool availability; missing LLVM/native tools are validation blockers, not a reason to weaken assertions.
- Manual audit: no `src-public/` production facade, duplicate backend IR, broad lowering API, or skip-only parity coverage was introduced.
- `git diff --check`
- `cabal build all && cabal test`

### Round Plan Record

Also written beside this plan:

- `orchestrator/rounds/round-241/selection-record.json`
- `orchestrator/rounds/round-241/round-plan-record.json`
