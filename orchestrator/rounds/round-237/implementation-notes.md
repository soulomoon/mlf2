### Changes Made
- `src/MLF/Elab/Run/Generalize/Prepare.hs`: removed the redundant `PreparedGeneralizationArtifact.pgaAcyclicBaseConstraint` field and updated the ownership note so the prepared base graph stays owner-local on `pgaBindParentsGa.gaBaseConstraint`.
- `src/MLF/Elab/Run/Pipeline.hs`: switched the elaboration/result-type run path to recover the prepared base constraint through `gaBaseConstraint bindParentsGa` instead of a duplicated outer-artifact field.
- `test/PipelineSpec.hs`: updated the generalization-preparation guard and the behavioral artifact test to prove the pipeline still uses the preserved base graph through `GaBindParents`.
- `test/RepoGuardSpec.hs`: added a milestone-6 repository guard that keeps `AGENTS.md`, `docs/architecture.md`, and the retained owner-local seam markers synchronized while asserting the retired `pgaAcyclicBaseConstraint` shim stays absent.
- `AGENTS.md`: aligned the durable type-level guidance with the audited retained seams by naming `mkUncheckedInstanceWitness` as the pre-normalization owner-local seam and `GaBindParents.gaBaseConstraint` as the generalization/result-type base-graph owner.
- `docs/architecture.md`: aligned the architecture contract with the same retained seams and the removal of the duplicated generalization-preparation base-graph field.
- `CHANGELOG.md`: recorded the milestone-6 integration cleanup outcome.

### Tests
- `git diff --check`: PASS
- `cabal build mlf2-test`: PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`: PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`: PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "assembly helper guard: generalization preparation owns shared artifact assembly"'`: PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`: PASS
- `cabal build all && cabal test`: PASS (`2573 examples, 0 failures`)
- Manual source audit: PASS. The bounded integration surface now classifies the remaining seams as accepted owner-local boundaries: mixed `NodeRef` storage with typed `NodeRefTag` boundaries, directional `to*Constraint` phase rephasers, pre-normalization `mkUncheckedInstanceWitness`, and `GaBindParents.gaBaseConstraint`. The only truly redundant broad shim in the audited surface was `pgaAcyclicBaseConstraint`, and it is retired here.
- Manual docs audit: PASS. `AGENTS.md`, `docs/architecture.md`, and `CHANGELOG.md` now describe the same accepted type-level safety story.
- Manual generated-file audit: PASS after restoring `runtime/mlfp_io/target/release/libmlfp_io.d` to its canonical tracked content so the round leaves no generated runtime depfile dirt.

### Notes
`orchestrator/state.json` was already modified in this worktree and is controller-owned, so it was left untouched.

The full gate emitted existing repository warnings in unrelated modules/imports and backend lowering exhaustiveness, but no new failures or new round-specific warning class appeared during this slice.
