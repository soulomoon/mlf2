# Round 024 Selection

Date: 2026-03-17
Round: `round-024`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
Controlling bug: `BUG-2026-03-16-001`

## Selected Roadmap Item

Roadmap item 1: execute the `R1` repair-boundary reproduction contract for `URI-R2-C1`.

## Why This Item Should Run Now

`orchestrator/state.json` is still parked at `stage: select-task` for `round-024`, with `current_task: null` and `retry: null`. That means no same-round retry is in force, so the guider must select the lowest-numbered unfinished roadmap item unless prior review artifacts require otherwise. No review artifact forces a retry here.

`orchestrator/roadmap.md` for the repair track lists `R1` through `R4` as pending, so `R1` is the next lawful item. The accepted predecessor chain already finalized the diagnostic gate: `D3` established one bounded repair-supporting direction at the localized `applyInstantiation` / `InstBot` boundary, and `D4` finalized `reopen-repair-track` in `round-023`. The repair-track design then requires the first implementation-side step to be a bounded reproduction contract before any repair attempt (`R2`) or verification (`R3`) can proceed.

`R1` must therefore run now to restate the locked failure in implementation-facing terms for the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` lane, while preserving the already-approved localization from `BUG-2026-03-16-001`, `D2`, and `D3`. This round must stay inside that single repair boundary and must not widen into broader replay work, alternate scenarios, fallback behavior, or a second executable interface.

## Round Scope Guard

This round is limited to roadmap item 1 only. It must remain inside the approved repair-track successor control plane and keep the active lane fixed to `URI-R2-C1` / `uri-r2-c1-only-v1`.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- admissible repair target remains `witness-replay/applyInstantiation-instbot-precondition` only;
- admissible owner boundary remains `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) only;
- inherited predecessor authority remains `P1 = pass`, `P2 = semantic-negative`, `D1 = pass`, `D2 = pass`, `D3 = pass`, `D4 = reopen-repair-track`;
- no production repair implementation is admissible until `R1` finalizes and a later round selects `R2`;
- no widened subject/scenario search, no compatibility fallback, no default-path behavior expansion, and no second executable interface are admissible.
