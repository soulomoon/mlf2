# Findings

## 2026-03-04
- Remaining row-2 adapters are still active in the current runtime:
  - `rtcSolvedCompat` field in `src/MLF/Elab/Run/ResultType/Types.hs`.
  - `rtcSolveLike` in `src/MLF/Elab/Run/ResultType/Types.hs`.
  - `ElabConfig.ecSolved` in `src/MLF/Elab/Elaborate.hs`.
- Current result-type view still materializes solved-domain structures via `rtvSolved` and solved rebuild paths in `src/MLF/Elab/Run/ResultType/View.hs`.
- Existing TODO already marks row-2 adapter retirement as the next planned target (`Task 33`), so this plan can align directly with existing repo priorities.
