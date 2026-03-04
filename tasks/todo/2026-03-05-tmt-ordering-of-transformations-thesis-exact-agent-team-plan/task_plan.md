# Task Plan: TMT Row Ordering of Transformations Thesis-Exact Agent-Team Plan

## Goal
Produce an execution-ready agent-team implementation plan that moves TMT row `Ordering of transformations` from `No` toward absolute thesis-exact alignment with thesis §12.1.3 and related translatability obligations.

## Scope
- Target row only: `Ordering of transformations` in `docs/notes/2026-02-27-transformation-mechanism-table.md`.
- Deliverable is a plan document under `docs/plans/` describing multi-wave agent-team implementation.
- No production behavior changes in this task.

## Current Phase
Phase 4 (Complete)

## Phases
| Phase | Description | Status |
|---|---|---|
| 1 | Re-audit thesis + code for row-specific divergence | complete |
| 2 | Draft agent-team implementation plan in `docs/plans/` | complete |
| 3 | Update task tracker artifacts (`task_plan.md`, `findings.md`, `progress.md`) | complete |
| 4 | Update rolling `TODO.md` priorities for next execution step | complete |

## Decisions Made
| Decision | Rationale |
|---|---|
| Use agent-team wave structure (RED guard -> core refactor -> integration -> verification -> docs closeout) | Matches established repo execution model and reduces merge conflicts. |
| Keep delayed weakening semantics explicitly in scope | Thesis §15.2.1 requires delayed weakening in normalized propagation witnesses. |
| Include strict gates with non-empty matcher evidence | Existing TMT closeouts rely on this verification discipline. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Parallel `cabal test` collisions in earlier audit step (`package.conf.inplace already exists`) | 1 | Rerun sequentially when verification is needed. |

## Notes
- Re-read this plan before finalizing docs edits.
- Keep proposed ownership boundaries explicit so worker agents can operate independently.
- Plan artifact: `docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md`.
