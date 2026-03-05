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
| 1. Setup task logs and collect baseline evidence | complete | Located table + thesis anchors + baseline statuses |
| 2. Round-based planner sweep (agent team) | complete | Round 1 stopped at mechanism 3 (`Ordering of transformations`) |
| 3. Planner artifact generation (agent team) | complete | Concrete plan + implementer prompt generated |
| 4. Implementer attempts (agent team) | complete | Attempt 1 completed with integrated team edits |
| 5. Verification + git actions + loop continuation | complete | Required row3 gate stack + full gate passed |
| 6. Final status report | in_progress | Produce concise loop log + terminal status |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `Phase 4 thesis-exact unification closure` boundary violation after removing flush-all fallback | 1 | Added owner-stamped pending-weaken provenance and strict closed-owner boundary scheduling; re-ran gate stack to green |

## Decisions
- Planner/implementer work is delegated via sub-agents.
- Row3 strict gap is treated as closed only when owner-stamped queue + strict boundary checks pass `row3 absolute thesis-exact guard` and `Phase 4 thesis-exact unification closure` together.
