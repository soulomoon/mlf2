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

## 2026-03-04 late re-audit (HEAD `17b2635`) — supersedes stale solved-handoff notes

- Thesis anchors remain unchanged:
  - Def. 15.3.12: edge translation chooses a propagation witness for each instantiation edge.
  - §15.3.6 + Fig. 15.3.5: elaboration is over translatable `χp` and proceeds by inductive term translation.
- Active runtime call chain is now `χp`-native end-to-end on production path:
  - `runPipelineElabWith` builds `generalizeAtWithView` from `PresolutionView` and passes `eePresolutionView` to `elaborateWithEnv` (`src/MLF/Elab/Run/Pipeline.hs:110-141`).
  - `reifyInst` calls `phiFromEdgeWitnessWithTrace` with `presolutionView` directly (no `chiSolved` handoff) (`src/MLF/Elab/Elaborate.hs:917-949`).
  - Active Φ entry/core signatures consume `GeneralizeAtWith` + `PresolutionView`; solved-typed signatures are confined to deprecated no-trace/test adapters (`src/MLF/Elab/Phi/Translate.hs:246-258`, `:284-317`).
- Table row classification remains `Thesis-exact = Yes` on current HEAD; row text and references were refreshed accordingly.
