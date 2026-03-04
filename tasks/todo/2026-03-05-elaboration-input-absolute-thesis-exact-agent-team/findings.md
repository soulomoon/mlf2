# Findings: Elaboration Input Absolute Thesis-Exact Planning

## 2026-03-05 Audit Findings
- Thesis contract (Def. 15.3.12, Sec. 15.3.6) expects elaboration input to be translatable-presolution driven with edge translation from chosen propagation witnesses.
- Current row is marked `Thesis-exact = Yes` under strict policy including test-only paths, but remaining internal surfaces still leave room to become more thesis-direct.

## Concrete Gaps To Target
- `src/MLF/Elab/Phi/Env.hs` still carries a `Solved` handle (`peResult`, `askResult`) in PhiM environment, even though active translation flow is `PresolutionView`-driven.
- `src/MLF/Elab/Phi/Translate.hs` still imports `fromSolved` and keeps `remapSchemeInfoM` on the solved-backed env path, while core flow already accepts `PresolutionView`.
- `src/MLF/Elab/Run/Scope.hs` documents a known deviation: `preferGenScope` swallows `bindingPathToRoot` errors (`Left _ -> ref`) rather than preserving explicit failure.
- `src/MLF/Elab/Phi/TestOnly.hs` retains synthetic trace construction (`phiFromEdgeWitnessAutoTrace`) for fixtures; this is practical but not fully thesis-native witness input.
- Guard tests currently focus on solved-typed API signatures and selected marker strings; they do not comprehensively guard the residual internal solved/env + synthetic-trace surfaces above.

## Planning Direction
- Use a wave plan with one RED guard wave, one parallel implementation wave (Teams B/C/D), and one integration/docs wave.
- Keep checked-authoritative and dual-path verification as mandatory non-regression gates.
