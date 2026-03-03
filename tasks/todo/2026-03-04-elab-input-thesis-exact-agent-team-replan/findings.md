# Findings — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Replan

- Thesis contract for this row is still `χp`-native elaboration with per-edge witness translation:
  - `papers/these-finale-english.txt` Def. 15.3.12
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5
- Current row remains non-thesis-exact in active runtime path:
  - `src/MLF/Elab/Elaborate.hs` still calls
    `phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`.
  - `src/MLF/Elab/Phi/Translate.hs` active trace entrypoints still require a `Solved` argument.
- Existing guard slices are present but one key guard is text-fragile:
  - `test/PipelineSpec.hs` checks for specific local binding text rather than the actual active call-site shape.
- New agent-team replan created:
  - `docs/plans/2026-03-04-elaboration-input-thesis-exact-agent-team-replan.md`
  - Includes team topology, wave sequencing, ownership boundaries, RED->GREEN gate matrix, and closeout criteria tied to active call-site removal + signature migration.
- Root tracker updated:
  - Added `Task 38` entry in `TODO.md` with links to the replan doc and task folder.
