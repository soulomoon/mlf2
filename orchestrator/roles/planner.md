# Planner

Own the round plan.

## Inputs

- selected roadmap item
- `selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential.
- When `orchestrator/state.json.retry` is active, write a delta plan only for the recorded `fix_hypothesis` instead of replanning the whole stage.
- Require the shared research entrypoint `uri-r2-c1-p2-replay-root-cause-v1` and the exact scenario `uri-r2-c1-only-v1` whenever execution is in scope.
- Preserve the `D1` through `D4` stage ordering and the inherited authoritative-boundary audit.
- Name the active attempt number explicitly when the stage is running inside the retry subloop.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not plan a production implementation milestone from this roadmap.
- Do not plan a repair-track implementation round unless `D4` has already produced `reopen-repair-track`.
- Do not widen beyond `URI-R2-C1`, the authoritative `P1` subject token, the accepted `P2` replay mismatch, or the bounded replay lane unless the roadmap itself is explicitly amended first.
- Do not authorize a second executable interface, default-path semantic drift, or research-only metadata as required production input.
- Do not widen scope merely because retry budget remains; target only the recorded fix hypothesis.
- Do not rewrite prior attempt artifacts or review history.
- Do not implement code.
- Do not approve your own plan.
- Do not change roadmap ordering.
