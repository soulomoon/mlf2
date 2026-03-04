# Task Plan: chi-p query-first elab/resulttype cleanup

## Goal
Execute `/docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md` Tasks 1-6 with TDD + verification gates while preserving checked-authoritative behavior.

## Phases
- [ ] Task 1 (Team A / Wave 0): Add red guard rails for internal solved materialization.
- [ ] Task 2 (Team A / Wave 0): Introduce shared `MLF.Elab.Run.ChiQuery` facade.
- [ ] Gate A: chi-first guard tests.
- [ ] Task 3 (Team B / Wave 1): Migrate ResultType internals to chi-first queries.
- [ ] Task 4 (Team C / Wave 1): Migrate Elaborate internals to chi-first queries.
- [ ] Gate B: ResultType + Phase 6 + chi-first slices.
- [ ] Task 5 (Team D / Wave 2): Integrate boundary and remove residual internal scaffolding.
- [ ] Task 6 (Teams D+E / Wave 2): Final verification + docs/changelog/todo/bugs updates.
- [ ] Gate C: `cabal build all && cabal test`.

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

## Notes
- Thesis source-of-truth remains `papers/these-finale-english.txt`; supplementary usage from `papers/xmlf.txt` only if thesis silent.
- Narrow solved compatibility adapters are allowed only where unavoidable and documented.
