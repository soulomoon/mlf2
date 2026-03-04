# Findings — 2026-03-04 TMT Elaboration Input Reaudit

- Thesis evidence:
  - Def. 15.3.12 defines per-edge translation `T(e)` from a chosen propagation witness and explicitly allows non-deterministic witness choice (`papers/these-finale-english.txt:14087-14097`).
  - §15.3.6 states elaboration is defined inductively over term shape for translatable presolution `χp` (`papers/these-finale-english.txt:14112-14117`).
- Code evidence:
  - Pipeline constructs `solvedForGen`, then projects to `presolutionViewForGen = fromSolved solvedForGen`; active elaboration config/env consume `PresolutionView` + χp-native callback (`src/MLF/Elab/Run/Pipeline.hs:109-141`).
  - Active Elaborate API is `ElabConfig` with `GeneralizeAtWith` and `ElabEnv` with `eePresolutionView` (no solved field in active path) (`src/MLF/Elab/Elaborate.hs:64-89`, `src/MLF/Elab/Elaborate.hs:181-205`).
  - `reifyInst` calls `phiFromEdgeWitnessWithTrace ... presolutionView ...` (`src/MLF/Elab/Elaborate.hs:917-949`).
  - Active Φ entrypoint/core signatures use `GeneralizeAtWith` + `PresolutionView`; solved-typed aliases are legacy no-trace fail-fast adapters (`src/MLF/Elab/Phi/Translate.hs:246-317`).
  - Facade comments classify no-trace/legacy Φ as test/debug-only (`src/MLF/Elab/Phi.hs:13-18`), and test suite guards active signatures against solved-typed callback regression (`test/ElaborationSpec.hs:331-349`, `test/PipelineSpec.hs:176-179`).
- Classification (strict policy):
  - Under strict criterion "no legacy solved-typed APIs anywhere, including test/debug", `Elaboration input` is `Thesis-exact = No` until legacy solved-typed APIs are removed.
  - Blocking artifacts include:
    - `GeneralizeAtWithLegacy` aliases (`src/MLF/Elab/Elaborate.hs:70-75`, `src/MLF/Elab/Phi/Translate.hs:253-258`)
    - legacy no-trace solved-typed entrypoints (`src/MLF/Elab/Phi/Translate.hs:262-282`, re-exported by `src/MLF/Elab/Phi.hs:16-18`)
    - solved-typed test-only callback surface (`src/MLF/Elab/Phi/TestOnly.hs:46-63`).
