# Task Plan: Row3 Ordering Absolute Thesis-Exact Agent-Team Replan

## Goal
Write a new implementation plan that drives TMT row `Ordering of transformations` from the current loop-final weaken flush behavior toward strict thesis-shaped boundary semantics (per thesis `SolveConstraint` ordering), while preserving existing regression/parity guarantees.

## Scope
- Planning only (no production implementation in this task).
- Re-audit thesis anchors and current row3 runtime behavior.
- Produce a new agent-team implementation plan with explicit wave sequencing, ownership boundaries, and verification gates.
- Update root `TODO.md` to track the new follow-up execution item.

## Current Phase
Phase 3 (complete)

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 1 | Re-audit thesis + row3 runtime gap | complete |
| 2 | Draft agent-team follow-up architecture and gates | complete |
| 3 | Write plan doc + sync trackers/TODO | complete |

## Decisions Made
| Decision | Rationale |
|---|---|
| Create a new follow-up plan file instead of overwriting the completed Task 44 plan | Preserve historical evidence trail and avoid mutating closed execution records. |
| Center the plan on pending-weaken ownership/provenance and boundary scheduling | Current blocker is scheduling safety (`OperationOnLockedNode` regressions) when flushing too early. |
| Keep strict RED->GREEN contract before code waves | Prevents repeating Task 44 behavior drift where correctness regressed during integration. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None | 1 | N/A |

## Deliverables
- `/Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row3-ordering-absolute-thesis-exact-agent-team-implementation-plan.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/findings.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/progress.md`
- `TODO.md` updated with a new follow-up task entry.
