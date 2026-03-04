# Progress — 2026-03-04 Update Elaboration Input Row

- Initialized task folder and planning files.
- Reviewed target table row in `docs/notes/2026-02-27-transformation-mechanism-table.md`.
- Inspected thesis references in `papers/these-finale-english.txt` around Def. 15.3.12 and §15.3.6.
- Inspected elaboration input runtime files:
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Phi/Translate.hs`
- Re-audited active Elaborate → Φ call path and confirmed residual solved handoff at
  `phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`.
- Patched the `Elaboration input` row to reflect boundary `χp` alignment plus active solved-argument dependency in Φ translation; updated row classification to `Thesis-exact = No`.
- Re-reviewed thesis anchors in `papers/these-finale-english.txt` around Def. 15.3.12 and §15.3.6/Fig. 15.3.5.
- Re-audited active runtime call chain on HEAD `ce8c392`:
  `runPipelineElabWith` -> `elaborateWithEnv` -> `reifyInst` ->
  `phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`.
- Refreshed the table row references to current line ranges and updated
  `Source revision` in
  `docs/notes/2026-02-27-transformation-mechanism-table.md`.
