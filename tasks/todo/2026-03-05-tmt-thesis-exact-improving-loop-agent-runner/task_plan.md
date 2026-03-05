# Task Plan: 2026-03-05 TMT Thesis-Exact Improving Loop Agent Runner

## Goal
Execute an autonomous improving loop against the Transformation Mechanism Table (Thesis vs Codebase), in fixed row order, using agent teams for planning and implementation, with strict YES/NO gates and terminal status constraints.

## Scope
- Repository: `/Volumes/src/mlf4`
- Ordered mechanisms:
  1. Elaboration input
  2. Result-type context wiring
  3. Ordering of transformations
  4. Per-edge propagation transform
  5. Graph operation execution (Graft/Merge/Weaken/Raise)
  6. Replay-map producer normalization (upfront strict contract)
  7. Replay-map consumer bridge in Phi
  8. Translatability normalization
  9. Canonicalization source used by Phi
  10. Identity reconciliation mechanism
  11. Non-root weaken/raise binder resolution
  12. Graph mutation during solve/presolution
  13. Dual-path verification mechanism
  14. Campaign classification status

## Hard Limits
- Max planning rounds: 10
- Max implementation attempts per round: 6

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Setup task logs and collect baseline evidence | complete | Locate table + thesis anchors + current status signals |
| 2. Round-based planner sweep (agent team) | complete | Stop at first NO each round |
| 3. Planner artifact generation (agent team) | complete | Plan + implementer prompt for selected mechanism |
| 4. Implementer attempts (agent team) | in_progress | Up to 6 attempts with YES/NO implementation gate |
| 5. Verification + git actions + loop continuation | pending | Commit/merge behavior per requested algorithm |
| 6. Final status report | pending | Emit concise log + FINAL STATUS line |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None yet | 0 | N/A |

## Decisions
- Use planner/implementer sub-agents (`spawn_agent`) for required team-based work.
- Use TMT row statuses plus direct code/thesis spot-check evidence for each round gate.
