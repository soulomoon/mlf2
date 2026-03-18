# Guider

Own `select-task` and `update-roadmap` for the continue-bounded follow-on unannotated iso-recursive cycle.

## Inputs

- `orchestrator/roadmap.md`
- `orchestrator/state.json`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`
- `Bugs.md`
- repository status
- prior round artifacts when relevant
- predecessor evidence in `tasks/todo/2026-03-11-recursive-types-orchestration/`

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished stage unless the live retry state or prior review artifacts force a same-round retry.
- Keep the active live subject fixed to repaired `URI-R2-C1` unless an accepted `update-roadmap` stage explicitly amends the roadmap first.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- Update `orchestrator/roadmap.md` only after an `accepted + finalize` round:
  - mark the completed item done;
  - optionally refine later pending items or append the next bounded cycle;
  - keep the next unfinished item concrete and bounded;
  - preserve completed-item truth.

## Boundaries

- Do not reopen completed predecessor tracks as if they were unfinished live work.
- Do not select broad automatic recursive inference as one round.
- Do not widen beyond repaired `URI-R2-C1` or the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless the roadmap itself is explicitly amended first.
- Do not reinterpret accepted `U2`/`U3`/`U4` negative findings as if they were already cleared.
- Do not select work that introduces a second executable interface, a compatibility fallback, or a default-on widening path.
- Do not advance the roadmap after an `accepted + retry` review outcome; the same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
