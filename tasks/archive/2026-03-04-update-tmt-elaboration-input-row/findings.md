# Findings — 2026-03-04 Update TMT Elaboration Input Row

- Thesis wording to preserve in row:
  - Def. 15.3.12 states translation of each instantiation edge `e` uses a chosen propagation witness `I`, and explicitly notes this choice is not deterministic.
  - §15.3.6 / Fig. 15.3.5 define elaboration inductively over term shape while using those per-edge translations.
- Codebase alignment for active path:
  - `runPipelineElabWith` builds `generalizeAtWithView` from `PresolutionView` and passes `eePresolutionView` into `elaborateWithEnv` (`src/MLF/Elab/Run/Pipeline.hs:110-141`).
  - `ElabEnv` carries `eePresolutionView`; active reification calls `phiFromEdgeWitnessWithTrace ... presolutionView ...` (`src/MLF/Elab/Elaborate.hs:64-89`, `src/MLF/Elab/Elaborate.hs:181-205`, `src/MLF/Elab/Elaborate.hs:917-949`).
  - Active Φ entry/core signatures are `GeneralizeAtWith -> PresolutionView` and do not take `Solved`; solved-typed Φ APIs are legacy no-trace adapters (`src/MLF/Elab/Phi/Translate.hs:246-317`).
