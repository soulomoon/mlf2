### Selected Extraction
- Milestone: Primitive Inventory Module
- Milestone id: milestone-1
- Direction id: direction-1a-primitive-inventory-module
- Extracted item id: item-1a-llvm-lowering-primitive-support-adapter
- Roadmap id: 2026-05-16-00-architecture-deepening-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001

### Goal

Make LLVM/native lowering consume `MLF.Primitive.Inventory` as the authority for lowerable reserved primitive names and primitive support coverage, while keeping native wrapper bodies, C-runtime symbol names, and eager sequencing implementation details private to `MLF.Backend.LLVM.Lower`.

### Approach

Milestone-1 remains the only dependency-ready unfinished milestone: it is already `in-progress`, has no dependencies, and milestones 2 through 6 are still blocked by `roadmap-view.json` dependencies. Round-238 created the private inventory owner and migrated frontend/backend typing adapters, but intentionally left broader lowering/runtime adapter alignment for later milestone-1 work.

Current HEAD still leaves `src/MLF/Backend/LLVM/Lower.hs` with a separate primitive support-name authority: `runtimeAndName`, the `io*Name` constants, and `ioPrimitiveNames` duplicate the reserved primitive names already present in `MLF.Primitive.Inventory.primitiveValueSpecs`. This round should move only the lowerer support-name policy to the inventory owner. It should not move wrapper/runtime implementation bodies out of the lowerer, add `BackendPrim`, create a public lowering API, change CLI emission preparation, or migrate the interpreter dispatcher in `MLF.Frontend.Program.Run`.

Keep the round serial. The selected change touches one shared lowering path plus inventory/docs/tests, and the acceptance evidence depends on integrated behavior rather than disjoint worker outputs.

### Steps

1. Audit the live primitive names and support categories in `src/MLF/Primitive/Inventory.hs` and `src/MLF/Backend/LLVM/Lower.hs`: the `__mlfp_and` lowering path, IO wrapper/entry functions, `ioPrimitiveNames`, `ioWrapperNames`, `lowerTyApp`, `resolveIOPrimitiveAsValue`, and the saturated global-call cases.
2. Add the minimal private inventory surface needed for lowering support policy. Prefer inventory-owned queries or fields such as "native boolean primitive name", "native IO primitive names", or "primitive native support kind" derived from `primitiveValueSpecs`; do not add a public facade or a second backend IR.
3. Rewire `MLF.Backend.LLVM.Lower` to import that inventory surface and derive its lowerable primitive-name checks from it. The lowerer may keep wrapper/entry function bodies, external C-runtime symbol names, closure allocation layout, and eager sequencing code locally, but any local implementation map must be keyed from inventory-owned primitive names and must fail or be covered if inventory support and lowerer implementation drift.
4. Remove or demote the duplicated lowerer name list as an authority. In particular, `ioPrimitiveNames` should no longer be a literal `Set.fromList` of reserved primitive names independent of `MLF.Primitive.Inventory`, and `runtimeAndName` should no longer be the source of truth for the `__mlfp_and` primitive name.
5. Add or tighten focused coverage so the inventory and lowerer agree on the native-lowerable primitive set. Use a production-internal or test-support seam if tests need to observe lowerer support coverage; do not widen `src-public/` or expose low-level lowering helpers only for tests.
6. Update `docs/architecture.md`, `docs/backend-native-pipeline.md`, and the existing repo guard wording so the primitive inventory owns shared primitive names/signatures and native support classification, while LLVM lowering owns only downstream wrapper/runtime implementation details. Avoid stale literal four-name "closed set" wording if the inventory contains the broader IO primitive set.
7. Keep `MLF.Frontend.Program.Run` interpreter runtime dispatch, CLI emission preparation, native parity policy, and milestone-2 callable-shape ownership out of scope unless a focused compile failure proves a minimal local adjustment is required.

### Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Primitive.Inventory"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive inventory ownership stays centralized across frontend and backend adapters"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "accepts primitive IO operations and emits native LLVM"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "links the backend-owned __mlfp_and runtime primitive in native mode"'`
- Run any additional focused `BackendLLVMSpec` selectors touched by the IO wrapper/name changes.
- Manual audit: no duplicate lowerer primitive support-name table remains outside `MLF.Primitive.Inventory`; lowerer-local wrapper/runtime implementation details remain private and are not presented as a second inventory owner.
- Manual audit: `docs/architecture.md`, `docs/backend-native-pipeline.md`, and `RepoGuardSpec` agree on the inventory/lowerer boundary and do not overclaim interpreter runtime or CLI emission migration.
- `git diff --check`
- `cabal build all && cabal test`

### Round Plan Record

Also written beside this plan:

- `orchestrator/rounds/round-239/selection-record.json`
- `orchestrator/rounds/round-239/round-plan-record.json`
