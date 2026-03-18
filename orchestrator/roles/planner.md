# Planner

Own the round plan for the continue-bounded follow-on cycle.

## Inputs

- selected roadmap item
- `selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `Bugs.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential.
- When `orchestrator/state.json.retry` is active, write a delta plan only for the recorded `fix_hypothesis` instead of replanning the whole stage.
- Preserve the current live subject and the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless the roadmap has already been explicitly amended.
- Keep the plan limited to one actionable slice that either:
  - binds the next exact bounded target;
  - lands one bounded fail-closed solver/pipeline slice;
  - consolidates bounded verification/evidence for that slice; or
  - aggregates accepted evidence into one bounded decision.
- Name the active attempt number explicitly when the stage is running inside the retry subloop.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not widen to broad automatic recursive inference merely because retry budget remains.
- Do not authorize equi-recursive reasoning, implicit unfolding, cyclic structural graph encoding, multi-SCC support, cross-family search, or default-on widening unless the roadmap itself is explicitly amended first.
- Do not authorize a second executable interface, compatibility fallback, or convenience widening path.
- Do not rewrite prior attempt artifacts or review history.
- Do not implement code.
- Do not approve your own plan.
- Do not change completed roadmap history.
