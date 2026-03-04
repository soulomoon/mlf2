# Findings: Row3 Ordering Absolute Thesis-Exact Replan

## Row3 Current State
- TMT row `Ordering of transformations` currently remains `Thesis-exact = No`.
- Runtime shape today:
  - Unification closure is drained at edge boundaries.
  - Delayed weakens are flushed at loop-final boundary (`runPresolutionLoop`).
  - Driver no longer performs a separate global `flushPendingWeakens`; post-loop stage is explicit finalization.

## Thesis Anchors
- `papers/these-finale-english.txt` §12.1.3 (Figure 12.1.1 `SolveConstraint`): initial unification solve, then per-edge propagation + unify in topological order.
- `papers/these-finale-english.txt` §15.2.1: delayed-weaken witness intent must be preserved.
- `papers/these-finale-english.txt` Def. 15.2.10: translatability obligations remain constructive constraints, not optional post-hoc checks.

## Known Regression History
- `BUG-2026-03-05-001` records that earlier per-edge weaken flushing caused:
  - `OperationOnLockedNode` failures
  - `make const` regression
  - `BUG-002-V1` regression
  - frozen parity drift
- Fix reverted to allowing intra-loop pending weakens and flushing at loop-final boundary.

## Planning Implication
- A viable follow-up must not simply move flush earlier; it needs ownership/provenance-aware scheduling so weakens are flushed when safe, while preserving thesis-shape momentum.
- RED guards must detect loop-final-only behavior and enforce new boundary contracts before implementation waves.

## Candidate High-Value Test Surfaces
- `test/PipelineSpec.hs`:
  - `row3 ordering thesis-exact guard`
  - `generalizes reused constructors via make const`
  - `checked-authoritative`
  - `Dual-path verification`
- `test/ElaborationSpec.hs`: `BUG-002-V1`
- `test/FrozenParitySpec.hs`: frozen baseline parity
- `test/Presolution/UnificationClosureSpec.hs`: Phase 4 closure/ordering characterization
- `test/TranslatablePresolutionSpec.hs`: translatability obligations
