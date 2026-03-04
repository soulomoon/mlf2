# Findings — 2026-03-04 Elaboration Input Absolute Thesis-Exact Audit

## Working Notes
- (pending)

## Findings
- Thesis contract for elaboration input is explicit:
  - Def. 15.3.12 defines instantiation-edge translation `T(e)` from a chosen propagation witness (non-deterministic witness choice).
  - §15.3.6 / Fig. 15.3.5 define elaboration inductively on term shape over translatable presolution `χp` and per-edge translations.
- Production path is `χp`-view driven:
  - Pipeline builds `presolutionViewForGen` and threads it through `ElabEnv.eePresolutionView` into `elaborateWithEnv`.
  - `MLF.Elab.Elaborate` and `MLF.Elab.Phi.Translate` active callback aliases are no longer solved-typed; `phiFromEdgeWitnessWithTrace` consumes `PresolutionView`.
- Residual strictness gap for "absolute thesis-exact" when including test-only paths:
  - `MLF.Elab.Phi.TestOnly` still exposes `Solved`-typed helper signatures (`phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and `phiFromEdgeWitnessAutoTrace`).
  - `phiFromEdgeWitnessAutoTrace` bridges through `fromSolved solved` and a synthetic trace for fixtures.
  - This does not affect production semantics, but it is not a thesis-native input surface under a strict all-paths criterion.
