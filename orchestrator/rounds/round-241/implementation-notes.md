### Changes Made
- `test/Parity/ProgramMatrix/NativePolicy.hs`: added the test-support owner for ProgramSpec-to-LLVM native/object-code parity policy. It derives rows from `programSpecToLLVMParityCases`, records source checking, interpreter/runtime, backend assembly, object-code, native-run, and tool availability layers, and reports diagnostics for duplicate, stale, missing, or overlapping classifications.
- `test/BackendLLVMSpec.hs`: rewired the ProgramSpec-to-LLVM parity block to consume `ProgramLLVMNativeParityPolicy` rows instead of local object-code/native-run string-list policy; unsupported backend/native rows now have explicit diagnostic assertion paths if introduced.
- `mlf2.cabal`: registered `Parity.ProgramMatrix.NativePolicy` in the `mlf2-test` stanza.
- `docs/backend-native-pipeline.md`: documented `Parity.ProgramMatrix.NativePolicy` as the test-support policy owner and clarified missing LLVM/native tools are validation blockers.
- `docs/architecture.md`: named the new test-support parity owner and the native/object-code layer split without widening production APIs.

### Tests
- `test/BackendLLVMSpec.hs`: verifies every interpreter-success row has exactly one native parity policy entry, no stale object-code/native classifications exist, and each policy row drives backend assembly, selected object-code smoke, native LLVM validation, native object-code validation, native executable run, and runtime result comparison.
- `test/ProgramSpec.hs`: unchanged; interpreter/runtime assertions continue to consume `programRuntimeSuccessCases`.
- `test/Parity/FrozenArtifacts.hs`: unchanged; frozen parity helpers remain unrelated to native/object-code row selection.

Validation run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix/classifies every interpreter-success case exactly once"'` passed, 1 example.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ProgramSpec-to-LLVM parity matrix"'` passed, 123 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'` passed, 122 examples.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'` passed, 1 example.
- `cabal build all && cabal test` passed, 2581 examples.

### Notes
No `src-public/` files or production native policy APIs were changed. The current ProgramSpec parity matrix has no native-unsupported exceptions, so `Parity.ProgramMatrix.NativePolicy` classifies all interpreter-success rows as native-run required and keeps native-unsupported policy data empty. Native test execution rewrites the tracked Rust dependency file `runtime/mlfp_io/target/release/libmlfp_io.d` with the local worktree path; that generated churn was restored after validation and is not part of the round diff.
