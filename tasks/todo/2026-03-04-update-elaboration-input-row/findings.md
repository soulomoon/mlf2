# Findings — 2026-03-04 Update Elaboration Input Row

- Thesis anchor confirmed in `papers/these-finale-english.txt`:
  - Def. 15.3.12: per-instantiation-edge translation selects a propagation witness for translation.
  - §15.3.6 + Fig. 15.3.5: elaboration is defined over a translatable presolution `χp` with witness-derived computations.
- Current runtime code path is boundary-`χp`-first but not fully `χp`-native internally:
  - `runPipelineElabWith` builds `ElabEnv` from `PresolutionView` + edge artifacts (`src/MLF/Elab/Run/Pipeline.hs`).
  - `ElabConfig`/`ElabEnv` active input shapes are `χp`-native (`GeneralizeAtWith`, `eePresolutionView`), but `reifyInst` still calls `phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)` (`src/MLF/Elab/Elaborate.hs`).
  - `phiFromEdgeWitnessWithTrace`/`phiFromEdgeWitnessCore` still take a `Solved` argument in the active trace path (`src/MLF/Elab/Phi/Translate.hs`), with adapter construction in `ChiQuery.chiSolved` (`src/MLF/Elab/Run/ChiQuery.hs`).
- Table row was refreshed to mark `Thesis-exact = No` for `Elaboration input`, and to call out the specific active solved handoff that remains.
- Re-audit on HEAD `ce8c392` confirms the same classification:
  - Thesis still anchors elaboration on translatable `χp` with chosen per-edge propagation witness (`papers/these-finale-english.txt` Def. 15.3.12, §15.3.6, Fig. 15.3.5).
  - Active code still passes a solved adapter at the term-path Φ call-site (`reifyInst -> phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`), and Φ trace entrypoints still require `Solved`.
  - Therefore row remains `Thesis-exact = No`; row text was refreshed with current line references and source revision.
