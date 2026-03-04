# Findings — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Replan

- Thesis contract retained for closeout docs:
  - `papers/these-finale-english.txt` Def. 15.3.12
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5
- Active runtime boundary is thesis-exact on current code:
  - pipeline builds/threads `generalizeAtWithView` + `eePresolutionView`:
    `src/MLF/Elab/Run/Pipeline.hs:112-141`
  - active elaboration boundary uses `GeneralizeAtWith` (no `Solved` in active
    callback type): `src/MLF/Elab/Elaborate.hs:64-80`
  - `reifyInst` passes `presolutionView` directly to
    `phiFromEdgeWitnessWithTrace`:
    `src/MLF/Elab/Elaborate.hs:917-949`
  - active Φ trace entry/core signatures are `PresolutionView`-native:
    `src/MLF/Elab/Phi/Translate.hs:284-317`
- Guard and verification references are aligned with closeout claims:
  - source guard for no active `ChiQuery.chiSolved` call-site:
    `test/PipelineSpec.hs:176-179`
  - source guard for no solved-typed active Elaborate/Phi callback aliases:
    `test/ElaborationSpec.hs:331-349`
  - representative checked-authoritative + dual-path slices:
    `test/PipelineSpec.hs:205-250`
- Wave 3 gate evidence captured for Wave 4 docs closeout:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)
