# Planner

Own the round plan for the post-`L2` automatic iso-recursive successor loop.

## Inputs

- selected roadmap item
- `selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `Bugs.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential.
- When `orchestrator/state.json.retry` is active, write a delta plan only for
  the recorded `fix_hypothesis` instead of replanning the whole stage.
- Treat the accepted `L1` / `L2` closure as binding continuity and preserve the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unless the roadmap has already
  been explicitly amended.
- Keep the plan limited to one actionable slice that either:
  - authorizes one lawful post-`L2` roadmap amendment;
  - selects one thesis-backed next live subject;
  - records one verifier-checkable safety/acceptance contract;
  - binds one exact bounded target;
  - lands one smallest safe design or implementation slice;
  - consolidates bounded verification/evidence for that slice; or
  - aggregates accepted evidence into one closure / successor decision.
- Name the active attempt number explicitly when the stage is running inside the
  retry subloop.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not widen to broad automatic recursive inference merely because retry
  budget remains.
- Do not authorize equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, second
  interfaces, or fallback paths unless the roadmap itself is explicitly amended
  first.
- Do not rewrite prior attempt artifacts or review history.
- Do not implement code.
- Do not approve your own plan.
- Do not change completed roadmap history.
