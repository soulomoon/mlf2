### Checks Run
- Command: `git status --short --branch`
  Result: pass. Worktree is on `orchestrator/round-241-native-parity-policy-module`; implementation diff is limited to docs, cabal test registration, `test/BackendLLVMSpec.hs`, the new test-support policy module, and round artifacts.
- Command: `git diff --check`
  Result: pass. No whitespace or conflict-marker issues.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix/classifies every interpreter-success case exactly once"'`
  Result: pass. 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix"'`
  Result: pass. 123 examples, 0 failures. The row names include source, interpreter/runtime, backend LLVM assembly, object-code, native-run, and tool availability classification.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`
  Result: pass. 122 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
  Result: pass. 1 example, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass. Full suite passed with 2581 examples, 0 failures.
- Command: `rg -n "llvmParityCoverage|llvmObjectCodeParityCases|llvmRequiredNativeRunParityCases|programSpecToLLVMParityCases|NativePolicy|nativeUnsupportedParityCase" test/BackendLLVMSpec.hs test/Parity test/ProgramSpec.hs test/FrozenParitySpec.hs test/Parity/FrozenArtifacts.hs test/LLVMToolSupport.hs docs/backend-native-pipeline.md docs/architecture.md`
  Result: pass. `BackendLLVMSpec` no longer owns the ProgramSpec-to-LLVM object-code/native-run policy; `Parity.ProgramMatrix.NativePolicy` owns the row policy beside `Parity.ProgramMatrix`.
- Command: `rg -n "src-public|MLF.API|MLF.Pipeline|NativePolicy|native.*policy" src-public src test`
  Result: pass. No production or `src-public/` native parity policy API was added.

### Plan Compliance
- Step 1, audit parity authorities: met. The implementation keeps `programSpecToLLVMParityCases` as the canonical interpreter-success input set, leaves `ProgramSpec` on `programRuntimeSuccessCases`, and records `Parity.FrozenArtifacts` as unrelated to native/object-code row selection.
- Step 2, add test-support parity policy owner beside `ProgramMatrix`: met. `test/Parity/ProgramMatrix/NativePolicy.hs` defines `ProgramLLVMNativeParityPolicy` with runtime case, case name/source, expected runtime result, source-checking status, interpreter/runtime status, backend LLVM assembly policy, object-code policy, native-run policy, and tool availability policy.
- Step 3, move manual backend parity string-list authority out of `BackendLLVMSpec`: met for the selected ProgramSpec-to-LLVM native/object-code policy. `BackendLLVMSpec` imports policy rows and diagnostics instead of local `llvmParityCoverage`, object-code, and required-native-run lists.
- Step 4, rewire ProgramSpec-to-LLVM parity block: met. `runLLVMParityPolicy` drives assembly validation, selected object-code smoke validation, native LLVM validation, native object-code validation, native executable execution, and runtime result comparison from the policy record.
- Step 5, preserve real assertions: met. Native-run-supported rows still compile, link, run, and compare exit code/stdout/stderr against `ProgramRuntimeExpectation`; unsupported branches assert the expected diagnostic if any are introduced.
- Step 6, add or tighten guard coverage: met. The classifier test fails on duplicate interpreter-success names, duplicate policy rows, stale object-code/native names, missing native classifications, and overlapping native-required/native-unsupported classifications.
- Step 7, update docs: met. `docs/backend-native-pipeline.md` and `docs/architecture.md` now name `Parity.ProgramMatrix.NativePolicy` and the layer split without widening production APIs.
- Step 8, keep frozen parity helper changes minimal: met. Frozen parity code was not changed; the focused frozen parity selector passed.

### Decision
**APPROVED**

### Evidence
The round stays inside `milestone-3` / `direction-3a-native-parity-policy-module` and creates one test-support owner beside `Parity.ProgramMatrix`. `BackendLLVMSpec` now consumes `ProgramLLVMNativeParityPolicy` rows for the ProgramSpec-to-LLVM native/object-code path, while interpreter/runtime assertions remain in `ProgramSpec`.

Every current interpreter-success policy row is classified exactly once and the focused parity matrix demonstrates visible layer classification in each example name. Tool absence remains a validation failure through `LLVMToolSupport` expectations rather than a skip-only path.

No `src-public/` files or production native policy APIs changed. `Parity.FrozenArtifacts` and frozen baseline behavior stayed unchanged. The validation run rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` as a generated side effect; that side effect was restored before this review artifact was written.

Milestone-3 completion is satisfied by this round: the native/object-code policy owner exists beside `ProgramMatrix`, backend/native coverage consumes it, skip/support classification is visible and guarded, real native assertions are preserved, and required validation passed. Status-only closeout can mark `milestone-3` `pending -> done` using the existing roadmap-view anchors.
