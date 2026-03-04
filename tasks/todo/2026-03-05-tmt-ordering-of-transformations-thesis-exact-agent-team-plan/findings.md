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
