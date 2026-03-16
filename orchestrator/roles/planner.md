# Planner

Own the round plan.

## Inputs

- selected roadmap item
- `selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`
- `Bugs.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential.
- When `orchestrator/state.json.retry` is active, write a delta plan only for the recorded `fix_hypothesis` instead of replanning the whole stage.
- Preserve the exact scenario `uri-r2-c1-only-v1` and the localized owner boundary for every repair stage.
- Preserve the `R1` through `R4` stage ordering and the inherited authoritative-boundary audit.
- Name the active attempt number explicitly when the stage is running inside the retry subloop.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not widen beyond `URI-R2-C1`, `uri-r2-c1-only-v1`, the localized `applyInstantiation` / `InstBot` owner boundary, or `BUG-2026-03-16-001` unless the roadmap itself is explicitly amended first.
- Do not authorize a second executable interface, compatibility fallback, or broad replay-semantics rewrite.
- Do not widen scope merely because retry budget remains; target only the recorded fix hypothesis.
- Do not rewrite prior attempt artifacts or review history.
- Do not implement code.
- Do not approve your own plan.
- Do not change roadmap ordering.
