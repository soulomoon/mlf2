# Findings

## 2026-03-04
- Thesis source confirms elaboration is defined over translatable presolution `χp` (Fig. 15.3.5, §15.3.6) and instantiation-edge translation `T(e)` is built from a chosen propagation witness with intentional non-determinism (Def. 15.3.12).
- Active production code path is `χp`-native:
  - `runPipelineElabWith` computes `presolutionViewForGen` and builds `generalizeAtWithBuilderView` over `PresolutionView`.
  - `ElabEnv` threads `eePresolutionView` into `elaborateWithEnv`.
  - Φ entrypoint `phiFromEdgeWitnessWithTrace` consumes `PresolutionView` and fails fast on missing trace (`MissingEdgeTrace`).
- Strict criterion blocker remains in test-only surfaces:
  - `MLF.Elab.Phi.TestOnly` helpers `phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and `phiFromEdgeWitnessAutoTrace` still take `Solved`.
  - `phiFromEdgeWitnessAutoTrace` still bridges with `fromSolved`.
  - Tests still call solved-typed helpers directly (`test/ElaborationSpec.hs`).
