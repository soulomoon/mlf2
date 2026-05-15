### Changes Made
- `src/MLF/Constraint/Types/Witness/Internal.hs`: split finalized witness construction from unchecked accumulation by introducing `ValidatedInstanceOps`, changing `mkInstanceWitness` to require that token, and adding explicit owner-local seams for post-normalization packaging and pre-normalization unchecked construction.
- `src/MLF/Constraint/Types/Witness.hs`: exposed the abstract `ValidatedInstanceOps` token on the narrowed production witness surface so finalized `InstanceWitness` construction no longer accepts arbitrary `[InstanceOp]`.
- `src/MLF/Constraint/Presolution/Witness.hs`: kept in-progress presolution witness assembly on an explicit unchecked owner-local seam and re-exported the normalization-owned validated-ops packaging helper for the finalized lane.
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`: rebuilt finalized production witnesses through `mkInstanceWitness (validatedInstanceOpsAfterNormalization ...)` only after `validateNormalizedWitness` succeeds on the normalization lane.
- `test/Presolution/WitnessSpec.hs`: strengthened the normalization regression to prove successful normalization can mint a finalized `InstanceWitness` through the validated-token seam.
- `test/PipelineSpec.hs`: added source guards for the new witness-construction split so the default constructor stays proof-token-based, pre-normalization assembly stays on the unchecked seam, and witness normalization validates before final packaging.
- `docs/architecture.md`: updated the witness-boundary note to describe the new finalized-token seam and the retained downstream owners for non-subsumed replay/Φ invariants.

### Tests
- `git diff --check`: passed.
- `cabal build mlf2-test`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalizeInstanceOpsFull produces validated witnesses when it succeeds"'`: passed (`2 examples, 0 failures`; property cases passed).
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization derives strict replay lane from edge semantics and maps codomain to edge-root replay binders"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization prunes no-replay non-root raise wrappers before Phi"'`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`: passed (`54 examples, 0 failures`).
- Manual source checks: confirmed `mkInstanceWitness :: ValidatedInstanceOps -> InstanceWitness` and `mkUncheckedInstanceWitness :: [InstanceOp] -> InstanceWitness` in `src/MLF/Constraint/Types/Witness/Internal.hs`; confirmed `buildEdgeWitness` uses `WitnessInternal.mkUncheckedInstanceWitness` in `src/MLF/Constraint/Presolution/Witness.hs`; confirmed `src/MLF/Constraint/Presolution/WitnessNorm.hs` still calls `validateNormalizedWitness envPost opsNormContract` before `mkInstanceWitness (Witness.validatedInstanceOpsAfterNormalization opsFinal)`.
- `cabal build all && cabal test`: passed (`2572 examples, 0 failures`).

### Notes
No downstream validation was removed in `MLF.Constraint.Presolution.Driver` or `MLF.Elab.Phi.Translate`. The new finalized-token seam subsumes raw-list construction on the post-normalization lane, but it does not subsume the broader trace-domain, replay-map, or Φ translatability invariants those downstream owners still check.

`orchestrator/state.json` was already dirty in this worktree and was not edited. `runtime/mlfp_io/target/release/libmlfp_io.d` is a generated build side effect from validation, not planned source work for this round.
