# Round 025 Selection

Date: 2026-03-17
Round: `round-025`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
Controlling bug: `BUG-2026-03-16-001`

## Selected Roadmap Item

Roadmap item 2: execute the `R2` bounded `InstBot` repair for `URI-R2-C1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `stage: select-task` for `round-025`, with `current_task: null` and `retry: null`. No same-round retry is active, so the guider must choose the lowest-numbered unfinished roadmap item unless prior review artifacts force otherwise. No live retry state or prior review artifact forces a return to `R1`.

`orchestrator/roadmap.md` marks item 1 (`R1`) as `done` and item 2 (`R2`) as the next `pending` stage. `round-024` finalized `R1` attempt 1 as the authoritative reproduction result, and its review record confirms that the locked `URI-R2-C1` / `uri-r2-c1-only-v1` failure is reproduced at the localized `applyInstantiation` / `InstBot` owner boundary without widening scope. With `D4 = reopen-repair-track` already accepted and `R1` complete, the next lawful step is the bounded production repair itself.

`R2` should therefore run now to attempt the smallest paper-faithful fix at `MLF.Elab.Inst.applyInstantiation` for the already-localized `InstBot` mismatch recorded in `BUG-2026-03-16-001`. This round must stay strictly inside the accepted repair lane and must not broaden into other subjects, scenarios, replay campaigns, fallback behavior, compatibility layers, or a second executable interface.

## Round Scope Guard

This round is limited to roadmap item 2 only. It must remain inside the approved repair-track successor control plane and keep the active lane fixed to `URI-R2-C1` / `uri-r2-c1-only-v1`.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- admissible repair target remains `witness-replay/applyInstantiation-instbot-precondition` only;
- admissible owner boundary remains `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) only;
- inherited predecessor authority remains `P1 = pass`, `P2 = semantic-negative`, `D1 = pass`, `D2 = pass`, `D3 = pass`, `D4 = reopen-repair-track`, and `R1 = pass`;
- this round may pursue one bounded repair at the localized owner boundary, but no widened replay search, no alternate scenario work, no compatibility fallback, and no second executable interface are admissible;
- `R3` verification and `R4` decision-gate work remain out of scope until `R2` finalizes.
