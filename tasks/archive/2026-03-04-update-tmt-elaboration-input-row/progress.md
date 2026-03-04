# Progress — 2026-03-04 Update TMT Elaboration Input Row

- Initialized task folder and planning files.
- Reviewed thesis sections around Def. 15.3.12 and §15.3.6 (`papers/these-finale-english.txt`) for exact elaboration semantics.
- Audited active runtime flow in:
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Phi/Translate.hs`
  - `test/PipelineSpec.hs`
  - `test/ElaborationSpec.hs`
- Updated only the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` with:
  - explicit thesis non-deterministic witness-choice wording
  - refreshed active-path and legacy-adapter description
  - current code/test references for boundary guards
- Sanity-checked the diff for markdown table integrity.
