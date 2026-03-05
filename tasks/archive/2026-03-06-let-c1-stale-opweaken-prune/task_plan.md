# Task Plan

- Status: completed
- Goal: prune stale non-root OpWeaken residue upstream in WitnessNorm so let-c1-apply-bool returns Int again without relaxing Omega strictness.
- Constraint: build on existing uncommitted PipelineSpec characterization changes in this worktree.
- Phases:
  1. Characterize the stale-vs-live non-root `OpWeaken` cases (`let-c1-apply-bool` vs `BUG-002-V4`). Completed.
  2. Refine `WitnessNorm` pruning so only dead producer residue is dropped. Completed.
  3. Rebaseline BUG-002 matrix expectations and rerun the validation gate. Completed.
- Decision log:
  - Rejected the initial over-broad “missing final binder domain => prune” rule because it regressed `BUG-002-V4`.
  - Kept non-root `OpWeaken` when the target is still root-local, still present in finalized source/replay binder domains, or still points at a fully abstract bound shape.
  - Dropped only stale producer residue: non-root `OpWeaken` with no finalized binder-domain membership and a concretized bound skeleton.
- Final verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bottom-int arrow"'`: PASS (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "let-c1-apply-bool"'`: PASS (`7 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'`: PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test`: PASS (`956 examples, 0 failures`)
