# Findings — 2026-03-04 Update Elaboration Input Row

- Thesis anchor confirmed in `papers/these-finale-english.txt`:
  - Def. 15.3.12: per-instantiation-edge translation selects a propagation witness for translation.
  - §15.3.6 + Fig. 15.3.5: elaboration is defined over a translatable presolution `χp` with witness-derived computations.
- Current runtime code path is boundary-`χp`-first but not fully `χp`-native internally:
  - `runPipelineElabWith` builds `ElabEnv` from `PresolutionView` + edge artifacts (`src/MLF/Elab/Run/Pipeline.hs`).
  - `elaborateWithEnv` still materializes solved adapter state via `ChiQuery.chiSolvedCompat` and feeds it through compat callbacks (`src/MLF/Elab/Elaborate.hs`).
  - Phi entry points still use solved-typed callback aliases (`GeneralizeAtWithCompat`) and `Solved` argument passing (`src/MLF/Elab/Phi/Translate.hs`).
- Table row was updated to reflect this as currently not thesis-exact for the active internal call chain (`Thesis-exact = No`), while preserving thesis anchors and concrete code references.
