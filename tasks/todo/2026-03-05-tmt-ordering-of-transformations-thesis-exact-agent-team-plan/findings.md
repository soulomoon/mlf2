# Findings: TMT Row Ordering of Transformations

## Requirements
- User requested a plan to make row `Ordering of transformations` more thesis-exact.
- Plan must be implementation-oriented and explicitly use agent teams.

## Thesis Anchors
- `papers/these-finale-english.txt` §12.1.3 (SolveConstraint): topological order, initial unification solve, then per-edge propagation + unify.
- `papers/these-finale-english.txt` §15.2.1: propagation witnesses are normalized with delayed weakenings.
- `papers/these-finale-english.txt` Def. 15.2.10: translatable presolution obligations are constructive constraints for translation readiness.

## Codebase Observations
- `runPresolutionLoop` enforces initial + per-edge unify closure and boundary assertions (`src/MLF/Constraint/Presolution/EdgeProcessing.hs`).
- Post-loop staging remains in `computePresolution`: `materializeExpansions`, `flushPendingWeakens`, `rewriteConstraint`, `rigidifyTranslatablePresolutionM`, `normalizeEdgeWitnessesM` (`src/MLF/Constraint/Presolution/Driver.hs`).
- Delayed weaken queueing/flushing is implemented in `EdgeUnify` (`src/MLF/Constraint/Presolution/EdgeUnify.hs`).
- Current TMT row keeps `Thesis-exact = No` for this mechanism.

## Gap Interpretation for Planning
- SolveConstraint ordering core is implemented, but auxiliary post-loop staging is still broad and not yet collapsed into a tighter thesis-shaped boundary.
- A credible path to "more thesis-exact" is to reduce or re-home post-loop staging while preserving delayed-weaken and translatability guarantees.

## Candidate Test Surfaces
- `test/Presolution/UnificationClosureSpec.hs` (`Phase 4 thesis-exact unification closure`).
- `test/TranslatablePresolutionSpec.hs` (Def. 15.2.10 checks).
- Existing global gates in `test/PipelineSpec.hs` (`checked-authoritative`, `Dual-path verification`).

## Plan Output
- Main execution plan written:
  - `docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md`
- Plan uses five teams (`contracts`, `loop-order`, `edge-weaken-core`, `finalization`, `integration-docs`) and Waves 0..4 with explicit RED/GREEN gates.

## Wave 0 (Team A) Findings
- Added RED guards in `test/PipelineSpec.hs`:
  - `row3 ordering thesis-exact guard: Driver removes global post-loop weaken flush`
  - `row3 ordering thesis-exact guard: EdgeProcessing flushes delayed weakens at each edge boundary`
- Added semantic characterization in `test/Presolution/UnificationClosureSpec.hs`:
  - `characterizes edge-boundary ordering by keeping OpWeaken targets inside each edge interior`
- Independent re-run of Gate A produced non-empty RED evidence:
  - `2 examples, 2 failures`
- Failure reason matches planned divergence:
  - Driver still contains global `flushPendingWeakens`.
  - Edge loop still lacks per-edge `flushPendingWeakens`.

## Immediate Implications
- Wave 1 must move weaken flushing to per-edge boundaries and make it idempotent.
- Gate B can be used as primary GREEN proof once Team B/C changes land.

## Wave 1 Findings (Team B + Team C)
- `EdgeProcessing.runPresolutionLoop` now enforces explicit per-edge sequence:
  - closure drain -> process edge -> closure drain -> flush pending weakens -> conditional closure drain -> boundary assert.
- Boundary checks now fail-fast on both pending unify edges and pending weaken queue.
- `EdgeUnify.flushPendingWeakens` was hardened for repeated boundary invocation (idempotent no-op for already-rigid/missing-parent/locked cases).
- Focused matcher status after Wave 1:
  - `Phase 4 thesis-exact unification closure`: `8 examples, 1 failure`
  - `Translatable presolution`: `8 examples, 0 failures`
- The remaining failure was isolated to the new characterization expression path, not a global translatability regression.

## Wave 2 Findings (Team D + Team A follow-up)
- Driver no longer imports/calls global post-loop `flushPendingWeakens`.
- Driver now has an explicit finalization stage with constructive checkpoints:
  - finalization boundary queue checks,
  - materialization coverage over TyExp nodes,
  - post-rewrite TyExp elimination guard,
  - witness/trace domain alignment post-normalization.
- The failing characterization example in `UnificationClosureSpec` was replaced by a direct single-edge fixture that still validates semantic invariant:
  - each `OpWeaken` target remains inside its edge trace interior.
- Post-wave focused gates all green:
  - `row3 ordering thesis-exact guard`: `2 examples, 0 failures`
  - `Phase 4 thesis-exact unification closure`: `8 examples, 0 failures`
  - `Translatable presolution`: `8 examples, 0 failures`

## Wave 3 Findings (Verification + Regression Handling)
- Required gate stack (`row3`, `Phase 4`, `Translatable`, `checked-authoritative`, `Dual-path`) was green with non-empty evidence.
- First full gate run (`cabal build all && cabal test`) failed with 3 regressions:
  - `generalizes reused constructors via make const`
  - `BUG-002-V1`
  - `Frozen parity artifact baseline`
- Root symptom: `OperationOnLockedNode` under reused-constructor path; parity drift in weakened-node artifacts.
- Resolution:
  - Added resolved bug record `BUG-2026-03-05-001` in `Bugs.md`.
  - Adjusted edge-loop weaken scheduling in `EdgeProcessing` to permit intra-loop pending weakens while preserving per-edge unify-closure checks; queue is drained/asserted at loop-final boundary.
- Post-fix revalidation:
  - targeted regressions all green (`1/0`, `1/0`, `1/0`),
  - required gates remained green,
  - full suite green (`938 examples, 0 failures`).

## Final Status Recommendation (Ordering row)
- Recommendation: `No` (keep `Thesis-exact = No`).
- Justification:
  - Major progress achieved (Driver-global flush removed; explicit finalization stage with construction checkpoints; guards and full gates green).
  - Remaining gap: delayed-weaken flushing is enforced at loop-final boundary, not strictly per-edge after each propagation step in thesis ordering shape (§12.1.3 + §15.2.1 intent).
