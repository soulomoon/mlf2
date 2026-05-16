### Changes Made
- `src/MLF/Primitive/Inventory.hs`: added the private primitive inventory owner for builtin type names/kinds, opaque builtin markers, source/backend primitive signatures, and qualification/canonicalization helpers shared by the frontend and backend adapters.
- `src/MLF/Frontend/Program/Builtins.hs`: rebuilt the frontend builtin registry as an adapter over `MLF.Primitive.Inventory` instead of owning a second builtin-type/primitive-signature table locally.
- `src/MLF/Frontend/Program/Resolve.hs`: switched builtin symbol seeding to the shared inventory owner so builtin type names and primitive value names stay synchronized with the frontend adapter and backend consumers.
- `src/MLF/Backend/IR.hs`: rebuilt backend runtime primitive typing from the shared primitive inventory and kept the backend-side opaque `IO` name check on the same canonical builtin-type path.
- `src/MLF/Backend/Convert.hs`: retired the local backend builtin-type list and backend primitive type table in favor of the shared inventory owner for builtin-type normalization, elaborated primitive typing, and closure-argument recovery.
- `test/PrimitiveInventorySpec.hs`, `test/BackendIRSpec.hs`, `test/BackendConvertSpec.hs`, `test/RepoGuardSpec.hs`, `test/Main.hs`, and `mlf2.cabal`: added focused coverage and wiring for the shared-owner contract, backend validation/adaptation, and repo-guard enforcement.
- `docs/architecture.md` and `docs/backend-native-pipeline.md`: documented `MLF.Primitive.Inventory` as the single private owner for shared builtin/primitive inventory data while keeping runtime-wrapper/native lowering details owner-local in the lowerer.
- `src/MLF/Primitive/Inventory.hs`: fixed the discovered `__mlfp_and` primitive signature regression so the shared owner preserves the expected `Bool -> Bool -> Bool` typing instead of collapsing to `Bool`.

### Tests
- `git diff --check`: PASS
- `cabal build mlf2-test`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "MLF.Primitive.Inventory"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "MLF.Backend.IR"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "accepts backend conversion when pure bindings reference IO primitives"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "accepts backend conversion when pure bindings reference IORef primitives"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "matches the checked backend IR snapshot for a primitive function program"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "typechecks direct IO bind primitive uses with consistent arguments"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "rejects constructor imports for opaque Prelude IO"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "primitive inventory ownership stays centralized across frontend and backend adapters"`: PASS
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary"`: PASS
- `cabal build all && cabal test`: PASS (`2579 examples, 0 failures`)

### Notes
`orchestrator/state.json` is controller-owned and was left untouched, matching the round instructions even though the parent controller already moved round-238 to the implement stage.

Parallel `cabal test` invocations tripped an ephemeral package-db race in `dist-newstyle`, so focused evidence was collected through the built `mlf2-test` executable sequentially instead of concurrent Cabal jobs.

The native-runtime full gate rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to the round worktree path as a build side effect; the tracked depfile content was restored before handoff so the round leaves only intentional source changes.
