# Findings — 2026-03-06 TMT Identity Row Update

## Initial scope
- Target row: `Identity reconciliation mechanism`
- Primary table: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Primary thesis source: `papers/these-finale-english.txt`
- Primary code path candidate: `src/MLF/Elab/Phi/IdentityBridge.hs`

## Evidence summary
- Thesis review (`papers/these-finale-english.txt` §§15.3.1-15.3.6) shows identity is carried directly by named nodes, computation contexts, and witness-derived computations (`ε`, `ϕR`, `T(e)`); there is no separate identity-reconciliation layer in the thesis presentation.
- Active code path is now much closer to that model:
  - `src/MLF/Elab/Phi/IdentityBridge.hs` restricts source identity to raw/copy/trace witness provenance and exact binder-key matching.
  - `src/MLF/Elab/Phi/Omega.hs` uses direct replay-target/spine membership checks for non-root binder resolution.
- Remaining codebase-level blocker for absolute row closure:
  - Production Φ identity handling is still split across `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/IdentityBridge.hs`, and `src/MLF/Elab/Phi/Omega.hs`, which together form an implementation-only runtime reconciliation subsystem absent from the thesis presentation.
  - `src/MLF/Elab/Phi/Binder.hs` still exposes canonical/base-key/copy-map reconciliation (`canonical`, `gaSolvedToBase`, `askCopyMap`, `askInvCopyMap`).
  - `src/MLF/Elab/Phi.hs` re-exports those helpers from the internal library surface.
- Conclusion: row `Identity reconciliation mechanism` remains `No`: semantics are now witness-domain exact, but compiled Φ code still contains a separate reconciliation mechanism beyond the thesis presentation.
