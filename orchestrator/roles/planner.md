# Planner

Own the round plan for the strategic automatic iso-recursive successor loop.

## Inputs

- selected roadmap item
- `selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential.
- When `orchestrator/state.json.retry` is active, write a delta plan only for
  the recorded `fix_hypothesis` instead of replanning the whole stage.
- Treat accepted rounds `001` through `081` as binding continuity and preserve
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unless the roadmap has already
  been explicitly amended.
- Keep the plan limited to one actionable slice that either:
  - defines capability and corpus;
  - audits architectural constraints;
  - generalizes accepted packet history into a mechanism map;
  - designs the search / ambiguity / termination model;
  - defines the reconstruction / validation contract;
  - runs a bounded coverage or feasibility campaign; or
  - aggregates accepted evidence into one architecture decision.
- Name the active attempt number explicitly when the stage is running inside the
  retry subloop.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not widen to broad automatic recursive inference merely because retry
  budget remains.
- Do not authorize equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, second
  interfaces, or fallback paths unless the roadmap itself is explicitly
  amended first.
- Do not rewrite prior attempt artifacts or review history.
- Do not implement code.
- Do not approve your own plan.
- Do not change completed roadmap history.
