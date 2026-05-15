### Changes Made
- `src/MLF/Constraint/Types/Witness/Internal.hs`: moved the raw `EdgeWitness` / `InstanceWitness` definitions and constructors behind an internal owner module so the default production surface no longer exposes raw witness construction as the normal path.
- `src/MLF/Constraint/Types/Witness.hs`: narrowed the production export surface to abstract witness types, read-side selectors, and validating smart constructors (`mkEdgeWitness`, `mkInstanceWitness`).
- `src/MLF/Constraint/Types/Witness/TestSupport.hs`: added an explicit test-only seam that re-exports raw witness constructors for intentionally malformed negative fixtures.
- `src/MLF/Constraint/Presolution/Witness.hs`: kept presolution witness assembly on the approved smart-constructor boundary.
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`: replaced `EdgeWitness` record-update reassembly with `mkEdgeWitness` so normalization stays inside the constructor boundary.
- `src/MLF/Constraint/Presolution/Rewrite.hs`: replaced `EdgeWitness` record-update reassembly in canonicalization with `mkEdgeWitness` so another live production rebuild path does not bypass the boundary.
- `src/MLF/Elab/Elaborate/Annotation.hs`, `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`: narrowed imports to the refined witness surface.
- `test/CanonicalizerSpec.hs`, `test/ElaborationSpec.hs`, `test/Presolution/MergeEmissionSpec.hs`, `test/Presolution/RaiseSpec.hs`, `test/Presolution/UnificationClosureSpec.hs`, `test/Presolution/WitnessSpec.hs`, `test/Thesis/ObligationPropertySpec.hs`: switched malformed fixture construction to the explicit test-only witness seam instead of the default production module.
- `test/PipelineSpec.hs`: added source-guard coverage that checks the default witness module no longer exports raw constructors, the test-only seam exists, and production reassembly paths use `mkEdgeWitness`.
- `test/RepoGuardSpec.hs`: forbids production imports of `MLF.Constraint.Types.Witness.TestSupport`.
- `mlf2.cabal`: registered the new witness internal/test-support modules in the appropriate stanzas.
- `docs/architecture.md`: updated the witness-constructor note so it matches the narrower production boundary and current downstream-validation split.

### Tests
- `git diff --check`: passed.
- `cabal build mlf2-test`: passed after one compile-fix pass to move a remaining malformed-fixture import in `test/Thesis/ObligationPropertySpec.hs` to `MLF.Constraint.Types.Witness.TestSupport`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 — Witness normalization"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`: passed.
- Manual source checks: confirmed `src/MLF/Constraint/Types/Witness.hs` no longer exports `EdgeWitness(..)` / `InstanceWitness(..)`, and confirmed `src/MLF/Constraint/Presolution/Witness.hs`, `src/MLF/Constraint/Presolution/WitnessNorm.hs`, and `src/MLF/Constraint/Presolution/Rewrite.hs` rebuild production witnesses through `mkEdgeWitness`.
- `cabal build all && cabal test`: passed (`2572 examples, 0 failures`).

### Notes
The scope stayed inside item `item-5a-production-constructor-boundary-freeze`: no broad fixture migration, no milestone closeout work, no public API widening, and no downstream validation removal beyond the constructor guarantees actually enforced at this seam.

`mkInstanceWitness` remains a thin construction seam. The selected round did not justify deleting downstream context-heavy witness validation that the constructor cannot yet prove on its own.

I initially tried running the three focused `cabal test` commands in parallel, but Cabal collided on inplace package state under the shared worktree. I reran those commands serially and kept the passing serial results above as the authoritative validation record.

`orchestrator/state.json` was already dirty in this worktree and was not edited. `runtime/mlfp_io/target/release/libmlfp_io.d` is a generated build side effect from validation, not a planned source change.
