# Progress Log

## 2026-02-17
- Audited API/export surfaces and confirmed legacy helper leak via `MLF.Elab.Pipeline` re-export of `expansionToInst`.
- Initialized task planning artifacts for this A3 cleanup.
- Removed `expansionToInst` from `src/MLF/Elab/Pipeline.hs` export list and import list to quarantine legacy conversion behind `MLF.Elab.Legacy`.
- Added explicit quarantine notes in `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs` imports.
- Updated tracking docs: marked A3 complete in `TODO.md`, added an Unreleased changelog line, and refreshed `implementation_notes.md`.
- Ran verification gate successfully: `cabal build all && cabal test`.
