### Changes Made
- `src/MLF/Backend/CallableShape.hs`: added the private callable-shape owner for shared direct-vs-closure head classification, including the shared callable-head datatypes and classifier.
- `src/MLF/Backend/IR.hs`: rewired backend validation and closure-global discovery to consume `MLF.Backend.CallableShape` through the IR-owned adapter instance while keeping `MLF.Backend.IR` as the executable backend IR seam.
- `src/MLF/Backend/Convert.hs`: routed closure-value detection and closure-head alias checks through the shared callable-shape owner, leaving conversion-local scope bookkeeping and closure-capture recovery local.
- `src/MLF/Backend/LLVM/Lower.hs`: routed lowering entry-point callable-head checks and closure-path selection through the shared callable-shape owner while keeping closure ABI and runtime-wrapper details local to lowering.
- `mlf2.cabal`: registered `MLF.Backend.CallableShape` in the private internal library.
- `docs/architecture.md`: documented `MLF.Backend.CallableShape` as the private callable-head owner shared by IR, conversion, and lowering.
- `test/RepoGuardSpec.hs`: extended the callable-shape guard to verify the new owner module and the updated architecture wording.

### Tests
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "validates explicit closure construction and indirect closure calls"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "clears shadowed closure locals when classifying let RHS values"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "classifies function-valued case pattern fields as closure locals"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "lowers case-selected closure callees through the explicit closure ABI"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects BackendApp heads that select closure values through let or case"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous"'`: passed.
- `git diff --check`: passed.
- `cabal build all && cabal test`: passed (`2581 examples, 0 failures`).

### Notes
No scope blockers remained after implementation. A brief attempt to run multiple `cabal test` focused targets in parallel hit the local `package.conf.inplace already exists` build-lock conflict, so the remaining focused targets were rerun sequentially and passed without code changes.
