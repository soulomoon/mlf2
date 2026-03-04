# Findings

## 2026-03-04
- Existing plan files in `docs/plans/` for this topic are stale relative to current code state; they still list already-retired blockers in `Elaborate`/`Phi.Translate`.
- Current strict-criterion blocker is concentrated in `src/MLF/Elab/Phi/TestOnly.hs`:
  - `phiFromEdgeWitnessNoTrace`, `phiFromEdgeWitness`, `phiFromEdgeWitnessAutoTrace` still take `Solved`.
  - `phiFromEdgeWitnessAutoTrace` still uses `fromSolved`.
- Current test dependency is concentrated in `test/ElaborationSpec.hs`, which still calls the solved-typed test-only helper signatures in many callsites.
- Active production elaboration/Phi runtime path is already `χp`-native (`PresolutionView` + trace-required `phiFromEdgeWitnessWithTrace`), so closeout work can focus on test-only compatibility retirement.
