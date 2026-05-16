### Changes Made
- `src/MLF/Primitive/Inventory.hs`: added `PrimitiveNativeSupport` and `PrimitiveIOOperation` so native-lowerable reserved primitives and IO support coverage are classified from the primitive inventory owner.
- `src/MLF/Backend/LLVM/Lower.hs`: rewired `__mlfp_and` and IO primitive support-name checks to consume `MLF.Primitive.Inventory`; wrapper bodies, C runtime symbols, closure layout, and eager sequencing remain lowerer-local. Added an internal coverage check so lowerer IO wrapper implementations match inventory-classified IO primitives.
- `src/MLF/Backend/IR.hs` and `src/MLF/Backend/Convert.hs`: updated row-5 module notes to point primitive name/support ownership at `MLF.Primitive.Inventory`.
- `docs/architecture.md`, `docs/backend-native-pipeline.md`, and `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`: updated primitive/native ownership wording to avoid the stale four-name closed-set claim and keep implementation details lowerer-local.
- `test/PrimitiveInventorySpec.hs`, `test/BackendLLVMSpec.hs`, and `test/RepoGuardSpec.hs`: added inventory native-support assertions, LLVM wrapper coverage evidence, and guard wording for the inventory/lowerer boundary.

### Tests
- `test/PrimitiveInventorySpec.hs`: verifies frontend builtin values and native-lowerable primitive support are derived from `MLF.Primitive.Inventory`.
- `test/BackendLLVMSpec.hs`: verifies native LLVM emits every inventory-classified IO wrapper and still links the backend-owned `__mlfp_and` primitive.
- `test/RepoGuardSpec.hs`: verifies frontend/backend adapters, including LLVM lowering, consume `MLF.Primitive.Inventory` and that lowerer-local literal support tables do not return.
- Focused validation:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Primitive.Inventory"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive inventory ownership stays centralized across frontend and backend adapters"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "accepts primitive IO operations and emits native LLVM"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "derives native IO wrapper coverage from the primitive inventory"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "links the backend-owned __mlfp_and runtime primitive in native mode"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "IO backend contract"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "backend-boundary mechanism table and closeout ledger stay synchronized"'`
- Baseline validation:
  - `git diff --check`
  - `cabal build all && cabal test` passed; full suite reported 2581 examples, 0 failures.

### Notes
Full native validation rewrites the tracked generated file `runtime/mlfp_io/target/release/libmlfp_io.d` with the worktree path. That generated change was restored and is not part of this round diff.
