# Task Plan — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Replan

## Goal
Write a fresh implementation plan (agent-team execution model) to make the `Elaboration input` row thesis-exact against current code and thesis anchors.

## Phases
- [completed] Re-audit thesis anchors and active runtime gap for row `Elaboration input`.
- [completed] Author a new agent-team implementation plan under `docs/plans/`.
- [completed] Update task findings/progress and root `TODO.md` with the new planning item.
- [completed] Validate the new plan references real matcher names and current call sites.

## Decisions
- Use `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12 and §15.3.6 as the thesis contract.
- Plan against current active call path (`runPipelineElabWith -> elaborateWithEnv -> reifyInst -> phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`).
- Require agent-team waves with explicit file ownership to avoid parallel-edit conflicts.

## Errors Encountered
- None.
