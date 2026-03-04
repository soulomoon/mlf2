# Findings: Elaboration Input Thesis-Exact

## 2026-03-04 Initial Findings
- Plan requires strict wave order: A -> (B + C) -> E -> D.
- Row cannot be flipped until all gates pass.
- Guard must explicitly forbid solved-typed signatures in `MLF.Elab.Phi.TestOnly`.
- Existing working tree is dirty; at least one target doc already modified and must be preserved/merged carefully.

## Wave 0 Verification
- Strict guards now read `src/MLF/Elab/Phi/TestOnly.hs` and assert solved-typed signatures are absent.
- Gate A is correctly RED before migration; failure mode is `expected False but got True` for solved-typed markers still present.

## Wave 1+2 Findings
- `MLF.Elab.Phi.TestOnly` now accepts chi-native inputs (`PresolutionView`) and no longer imports `Solved`/`fromSolved`.
- Elaborator tests now pass `presolutionViewFromSolved solved` at auto-trace helper callsites; no-trace helper no longer receives solved positional arg.
- All required gates are green, satisfying prerequisite for TMT row flip.

## Wave 3 Findings
- Strict-policy table note and row evidence now explicitly include test-only path scope.
- Row `Elaboration input` classification updated to `Yes` only after Gate B, Gate C slices, and final gate were all green.
- Changelog/TODO/implementation notes now reflect closure and next priorities.
