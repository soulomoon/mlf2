# Task Plan: Elaboration Input Witness-Authoritative Agent-Team Plan

## Metadata
- Date: 2026-03-05
- Repo: /Volumes/src/mlf4
- Target row: `docs/notes/2026-02-27-transformation-mechanism-table.md` -> `Elaboration input`
- Plan doc: `/Volumes/src/mlf4/docs/plans/2026-03-05-elaboration-input-witness-authoritative-agent-team-implementation-plan.md`
- Objective: Write an execution-ready agent-team implementation plan to make elaboration input more thesis-exact by retiring remaining fallback synthesis in the active path.

## Scope
- Re-audit thesis anchors and current code for row-1 (`Elaboration input`) only.
- Produce a wave-based team plan with strict ownership and RED->GREEN gates.
- Initialize tracking artifacts (`task_plan.md`, `findings.md`, `progress.md`).
- Update root `TODO.md` with the new planning item.

## Current Phase
Planning complete; awaiting execution selection.

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 0 | Load required skills and re-audit row + code anchors | complete |
| 1 | Draft agent-team implementation plan in `docs/plans/` | complete |
| 2 | Initialize task tracking artifacts under `tasks/todo/...` | complete |
| 3 | Sync root `TODO.md` with new plan priority | complete |
| 4 | Ready for execution with agent teams | complete |

## Decisions
| Decision | Rationale |
|---|---|
| Treat this as a fresh strictness wave despite prior "absolute" closeout | Live code re-audit found additional fallback mechanisms not covered by prior absolute guard markers. |
| Keep thesis criterion strict (includes test-only paths) | Matches table policy and avoids partial claims. |
| Use Team A-E wave topology with Wave 1 parallelization | Aligns with user request to implement via agent teams while preserving ownership boundaries. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None | 0 | N/A |

## Gate Contract (for future execution)
- RED baseline:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative guard"'`
- GREEN gates:
  - `--match "elab-input witness-authoritative guard"`
  - `--match "elab-input absolute thesis-exact guard"`
  - `--match "checked-authoritative"`
  - `--match "Dual-path verification"`
  - `cabal build all && cabal test`
