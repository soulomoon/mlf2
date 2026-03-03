# Findings — 2026-03-04 TMT Elaboration Input Thesis-Exact Remediation Plan

- Current row is correctly `No`: active elaboration still materializes `solvedCompat = ChiQuery.chiSolvedCompat presolutionView` in `src/MLF/Elab/Elaborate.hs`.
- Active Phi/generalize callback path still has solved-typed compatibility signatures in `src/MLF/Elab/Phi/Translate.hs` (`GeneralizeAtWithCompat`, `phiFromEdgeWitnessWithTrace` taking `Solved`).
- Existing guards are insufficiently strict:
  - `test/PipelineSpec.hs` checks absence of `chiSolved` but not `chiSolvedCompat`.
  - `test/ElaborationSpec.hs` solved-typed alias check pattern does not match the actual compat alias shape.
- Thesis anchors for this row remain: `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12 and §15.3.6.
- Wave 0 guard hardening is landed:
  - `test/PipelineSpec.hs` now fails if `chiSolvedCompat presolutionView` appears in `Elaborate.hs`.
  - `test/ElaborationSpec.hs` now fails if active aliases/signatures retain solved-typed compat shape (`GeneralizeAtWithCompat`, solved-typed `phiFromEdgeWitnessWithTrace` args).
