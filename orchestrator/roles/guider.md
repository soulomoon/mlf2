# Guider

Own `select-task` and `update-roadmap` for the `URI-R2-C1` replay root-cause track.

## Inputs

- `orchestrator/roadmap.md`
- `orchestrator/state.json`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
- repository status
- prior round artifacts when relevant
- predecessor evidence in `tasks/todo/2026-03-11-recursive-types-orchestration/`

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished stage unless the live retry state or prior review artifacts force a same-round retry.
- Keep the active scenario fixed to `URI-R2-C1` and `uri-r2-c1-only-v1`.
- Keep the inherited authoritative `P1` subject token and accepted `P2` replay failure as the only admissible root-cause inputs.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- Update `orchestrator/roadmap.md` only after an `accepted + finalize` round and mark the completed item.

## Boundaries

- Do not reopen the completed `P1` through `P4` prototype-evidence track as if it were unfinished work.
- Do not select a production implementation round from this roadmap.
- Do not widen beyond `URI-R2-C1`, the authoritative `P1` subject token, or the bounded `P2` replay-failure lane unless the roadmap itself is explicitly amended first.
- Do not select work that introduces a second executable interface or treats prototype output as default production behavior.
- Do not advance the roadmap after an `accepted + retry` review outcome; the same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
