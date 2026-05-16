### Checks Run
- Command: `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-239 diff --check`
  Result: pass; no whitespace errors or malformed patch hunks.
- Command: `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-239 diff --stat`
  Result: pass; scope is limited to milestone-1 code/docs/tests plus the round artifact directory.
- Command: `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-239 diff -- src/MLF/Primitive/Inventory.hs src/MLF/Backend/LLVM/Lower.hs src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs`
  Result: pass; lowering now reads primitive lowerability names from `MLF.Primitive.Inventory`, while wrapper bodies, C runtime symbol names, closure layout, and eager sequencing remain in `MLF.Backend.LLVM.Lower`.
- Command: `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-239 diff -- test/PrimitiveInventorySpec.hs test/BackendLLVMSpec.hs test/RepoGuardSpec.hs docs/architecture.md docs/backend-native-pipeline.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
  Result: pass; focused tests/docs/RepoGuard wording were updated to the same owner boundary and do not claim interpreter or CLI migration.
- Command: `rg -n '^### \[in-progress\] 1\. Primitive Inventory Module$|^#### Completion Pointers: milestone-1$|^## Completed Rounds$' /Volumes/src/mlf4/orchestrator/worktrees/round-239/orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001/roadmap.md /Volumes/src/mlf4/orchestrator/worktrees/round-239/orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/roadmap-history.md`
  Result: pass; the `milestone-1`, `milestone-1-completion`, and `roadmap-history-completed-rounds` anchors referenced for status-only closeout all resolve.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Primitive.Inventory"'`
  Result: pass; 2 examples, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive inventory ownership stays centralized across frontend and backend adapters"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "accepts primitive IO operations and emits native LLVM"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "derives native IO wrapper coverage from the primitive inventory"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "links the backend-owned __mlfp_and runtime primitive in native mode"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "IO backend contract"'`
  Result: pass; 17 examples, 0 failures, including the broader native IO runtime rows touched by the wrapper/name changes.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal test mlf2-test --test-show-details=direct --test-options='--match "backend-boundary mechanism table and closeout ledger stay synchronized"'`
  Result: pass; 1 example, 0 failures.
- Command: `cd /Volumes/src/mlf4/orchestrator/worktrees/round-239 && cabal build all && cabal test`
  Result: pass; full repo gate passed with 2581 examples and 0 failures. This validation also rewrote tracked `runtime/mlfp_io/target/release/libmlfp_io.d` with worktree-local path data.

### Plan Compliance
- Audit the live primitive names and support categories in `MLF.Primitive.Inventory` and `MLF.Backend.LLVM.Lower`: met. The diff moved `runtimeAndName` and `ioPrimitiveNames` to inventory-owned queries and added a lowerer-local coverage check for IO wrapper implementations.
- Add the minimal private inventory surface needed for lowering support policy: met. `PrimitiveNativeSupport`, `PrimitiveIOOperation`, `nativeAndPrimitiveName`, `nativeIOPrimitiveName`, and `nativeIOPrimitiveNames` stay inside the private inventory module and no public backend facade was added.
- Rewire `MLF.Backend.LLVM.Lower` to derive lowerable primitive-name checks from inventory while keeping implementation details local: met. Lowering now consumes inventory authority for the lowerable reserved names, but wrapper bodies, C runtime symbol names, closure allocation layout, and eager sequencing code remain in the lowerer.
- Remove or demote the duplicated lowerer name list as an authority: met. `ioPrimitiveNames` is no longer a literal `Set.fromList`, and `runtimeAndName` no longer hardcodes `__mlfp_and`.
- Add or tighten focused coverage for inventory/lowerer agreement: met. `PrimitiveInventorySpec`, `BackendLLVMSpec`, and `RepoGuardSpec` now cover native support classification, wrapper coverage, and guard against reintroducing local lowering authority tables.
- Update docs and RepoGuard wording without overclaiming other migrations: met. `docs/architecture.md`, `docs/backend-native-pipeline.md`, the backend-boundary mechanism table, and RepoGuard all describe inventory ownership only for shared primitive names/support classification and keep interpreter/CLI/native parity migration claims out of scope.
- Keep interpreter runtime dispatch, CLI emission preparation, parity policy, and callable-shape ownership out of scope: met. No `src-public/` files changed, no CLI adapter was introduced, and the diff stays within milestone-1 primitive-inventory ownership.

### Decision
**APPROVED**

### Evidence
No implementation findings.

`MLF.Primitive.Inventory` is now the authority for the lowerable reserved primitive-name/support surface used by LLVM/native lowering. `MLF.Backend.LLVM.Lower` still owns only downstream implementation details: wrapper bodies, emitted entry/wrapper functions, native C runtime symbol calls, closure layout, and eager execution mechanics. The added `checkedIOPrimitiveImplementations` guard makes lowerer drift from the inventory-classified IO primitive set fail immediately.

The milestone-1 completion signal is satisfied at the current head: frontend builtins, backend IR, checked-program conversion, and LLVM lowering all consume the inventory owner instead of maintaining competing primitive tables; docs record that owner boundary; focused primitive/native checks pass; and the full `cabal build all && cabal test` gate is green. Marking `milestone-1` `done` is a status-only closeout because it does not change future coordination meaning, sequencing, or verification rules; it only records that the last milestone-1 adapter gap named by the roadmap is now closed.

Residual operational note: the full native validation re-touched tracked `runtime/mlfp_io/target/release/libmlfp_io.d` with worktree-local path content. That generated file is not evidence for the primitive-inventory adapter round and should not be treated as part of the approved scope.
