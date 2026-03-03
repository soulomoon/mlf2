# Task Plan: chi-p query-first elab/resulttype cleanup

## Goal
Execute `/docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md` Tasks 1-6 with TDD + verification gates while preserving checked-authoritative behavior.

## Phases
- [x] Task 1 (Team A / Wave 0): Add red guard rails for internal solved materialization.
- [x] Task 2 (Team A / Wave 0): Introduce shared `MLF.Elab.Run.ChiQuery` facade.
- [x] Gate A: chi-first guard tests.
- [x] Task 3 (Team B / Wave 1): Migrate ResultType internals to chi-first queries.
- [x] Task 4 (Team C / Wave 1): Migrate Elaborate internals to chi-first queries.
- [x] Gate B: ResultType + Phase 6 + chi-first slices.
- [x] Task 5 (Team D / Wave 2): Integrate boundary and remove residual internal scaffolding.
- [x] Task 6 (Teams D+E / Wave 2): Final verification + docs/changelog/todo/bugs updates.
- [x] Gate C: `cabal build all && cabal test`.

## Team Ownership Mapping
- Team A (`query-layer`): Tasks 1-2
- Team B (`resulttype`): Task 3
- Team C (`elab-core`): Task 4
- Team D (`integrator`): Tasks 5-6 integration/doc wiring
- Team E (`reviewer`): gate verification and review assertions

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-03 | `tmux attach` terminal clear support issue | 1 | Retried with `TERM=xterm-256color`, attached + detached successfully |
| 2026-03-03 | shell reported `mkdir` not found in one command | 1 | Retried with explicit `/bin/mkdir` and succeeded |
| 2026-03-03 | transient Cabal lock race while running overlapping test commands | 1 | Re-ran gate commands sequentially in a single shell; all target slices passed deterministically |

## Notes
- Thesis source-of-truth remains `papers/these-finale-english.txt`; supplementary usage from `papers/xmlf.txt` only if thesis silent.
- Narrow solved compatibility adapters are allowed only where unavoidable and documented.

## Verification Evidence Checklist
- [x] Gate A command output captured (`--match "chi-first guard"`).
- [x] Gate B command output captured (`--match "ResultType|Phase 6 — Elaborate|chi-first"`).
- [x] Integration slice output captured (`--match "Pipeline \(Phases 1-5\)|Dual-path verification|chi-first integration"`).
- [x] Full Gate C output captured (`cabal build all && cabal test`).
- [x] Final closeout slice captured (`--match "Phase 6 — Elaborate|ResultType|Dual-path verification"`).

## Completion Summary
- Commits delivered for Tasks 1-5:
  - `38568c7` test guardrails
  - `a8505c6` chi-query facade
  - `9bd5959` result-type chi-first migration
  - `6ff9643` elaborate chi-first migration
  - `b8df77a` pipeline/result-type boundary integration
- Task 6 closeout updates:
  - docs/notes + implementation notes + changelog + TODO synchronized with chi-first migration and remaining solved-compat boundaries.
  - task folder prepared for archive move after final verification run.
