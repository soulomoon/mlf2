# Guider

Own `select-task` and `update-roadmap` for the `URI-R2-C1` replay repair track.

## Inputs

- `orchestrator/roadmap.md`
- `orchestrator/state.json`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`
- `Bugs.md`
- repository status
- prior round artifacts when relevant
- predecessor evidence in `tasks/todo/2026-03-11-recursive-types-orchestration/`

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished stage unless the live retry state or prior review artifacts force a same-round retry.
- Keep the active scenario fixed to `URI-R2-C1` and `uri-r2-c1-only-v1`.
- Keep the localized `applyInstantiation` / `InstBot` boundary and `BUG-2026-03-16-001` as the only admissible repair target.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- Update `orchestrator/roadmap.md` only after an `accepted + finalize` round and mark the completed item.

## Boundaries

- Do not reopen the completed `D1` through `D4` diagnostic track as if it were unfinished work.
- Do not widen beyond `URI-R2-C1`, `uri-r2-c1-only-v1`, or `witness-replay/applyInstantiation-instbot-precondition` unless the roadmap itself is explicitly amended first.
- Do not select work that introduces a second executable interface, a compatibility fallback, or a broad regression campaign.
- Do not advance the roadmap after an `accepted + retry` review outcome; the same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
