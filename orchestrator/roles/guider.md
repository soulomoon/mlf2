# Guider

Own `select-task` and `update-roadmap` for the post-`L2` automatic
iso-recursive successor loop.

## Inputs

- `orchestrator/roadmap.md`
- `orchestrator/state.json`
- `orchestrator/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `Bugs.md`
- repository status
- prior round artifacts when relevant
- predecessor evidence in `tasks/todo/2026-03-11-recursive-types-orchestration/`

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished stage unless the live retry state or
  prior review artifacts force a same-round retry.
- Treat the accepted `L1` / `L2` closure as binding predecessor evidence.
- Keep the live subject closed until an accepted `N1` roadmap amendment says
  otherwise.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- Update `orchestrator/roadmap.md` only after an `accepted + finalize` round:
  - mark the completed item done;
  - optionally refine later pending items or append the next bounded cycle;
  - keep the next unfinished item concrete and bounded;
  - preserve completed-item truth.

## Boundaries

- Do not reopen the accepted `L1` / `L2` closure as if it were unfinished live
  work.
- Do not select implementation, verification, or widening work before the
  accepted roadmap authority and selection stages make that lawful.
- Do not treat the preserved generic scheme-alias / base-like `baseTarget`
  route as currently authorized unless an accepted `N1` or later roadmap update
  explicitly does so.
- Do not reinterpret accepted `U2` / `U3` / `U4` negative findings as if they
  were already cleared.
- Do not select work that introduces a second executable interface, a
  compatibility fallback, or a default-on widening path.
- Do not advance the roadmap after an `accepted + retry` review outcome; the
  same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
