# Task Plan: Close Review Findings Runtime Replay/Mediation Removal

## Metadata
- Date: 2026-03-03
- Owner: codex-main (with agent team)
- Source plan: `docs/plans/2026-03-03-close-review-findings-runtime-replay-mediation-removal-agent-team-plan.md`

## Goal
Execute the agent-team implementation plan to remove runtime replay/mediation behavior from production run paths, harden semantic guard tests, restore validation signaling, and align docs after green gates.

## Phases
1. [completed] Setup and context capture
2. [completed] Stage 1 hardening (semantic tests + validation behavior)
3. [completed] Stage 2 runtime cutover (shared finalization API + replay-free run path)
4. [completed] Stage 3 docs/changelog consistency
5. [completed] Verification gates (A/B/C + full build/test)

## Team Allocation
- Team A/B: runtime boundary + shared finalization
- Team C: result-type solved validation path
- Team D: tests/guards migration
- Team E/F: docs and independent review (controller pass)

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `sed: src/MLF/Elab/Run/Types.hs: No such file or directory` | 1 | Corrected target path to `src/MLF/Elab/Run/ResultType/Types.hs`. |
| `tmux attach -t mlf4-close-review` failed (`terminal does not support clear`) | 1 | Kept tmux session scaffold and continued with in-session worker agents for execution. |
| `migration guardrail: thesis-core boundary matches legacy outcome` fails (canonical map mismatch) | 1 | Root-caused to legacy eliminated-node-only canonical-map metadata; guardrail now compares canonical maps on shared live-node domain and keeps strict solved-query parity checks. |
| `Phase 6 — Elaborate` regressions after replay-free cutover (`PhiInvariantError` / `PhiTranslatabilityError`) | 1 | Restored full snapshot-finalization semantics in `MLF.Constraint.Finalize` via shared `Solve.finalizeConstraintWithUF`; targeted + full gates now green. |
| Parallel `cabal test` runs on same test target failed with log unlink error (`removeLink ... mlf2-test.log`) | 1 | Reran the target sequentially (no code change); test passed. |
