# Round 026 Selection

Date: 2026-03-17
Round: `round-026`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
Controlling bug: `BUG-2026-03-16-001`

## Selected Roadmap Item

Roadmap item 3: execute the `R3` locked replay-path verification for `URI-R2-C1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `stage: select-task` for `round-026`, with `current_task: null` and `retry: null`, so there is no live same-round retry forcing the guider back into an earlier stage. Under the guider contract, the next selection should therefore be the lowest-numbered unfinished roadmap item unless prior review artifacts require otherwise.

`orchestrator/roadmap.md` marks items 1 (`R1`) and 2 (`R2`) as `done`, while item 3 (`R3`) is the first remaining `pending` stage. The authoritative review artifacts confirm that progression: `round-024/review-record.json` finalized `R1` as `pass`, and `round-025/review-record.json` finalized `R2` attempt 3 as the authoritative `pass` for the bounded `InstBot` repair. The `round-025/review.md` record also says the locked `URI-R2-C1` replay success assertion now exists and that the full `cabal build all && cabal test` gate is green, which is exactly the prerequisite state for the dedicated replay verification stage.

`R3` should therefore run now to verify, on the locked `URI-R2-C1` / `uri-r2-c1-only-v1` lane only, that the authoritative replay path succeeds after the bounded `applyInstantiation` / `InstBot` repair and that no prohibited widening, fallback behavior, second executable interface, or broader campaign drift was introduced. The immutable predecessor packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains evidence only and does not reopen or displace this bounded repair-track progression.

## Round Scope Guard

This round is limited to roadmap item 3 only. It must remain inside the approved repair-track successor control plane and keep the active lane fixed to `URI-R2-C1` / `uri-r2-c1-only-v1`.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- admissible repair target remains `witness-replay/applyInstantiation-instbot-precondition` only;
- admissible owner boundary remains `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) only;
- inherited predecessor authority remains `P1 = pass`, `P2 = semantic-negative`, `D1 = pass`, `D2 = pass`, `D3 = pass`, `D4 = reopen-repair-track`, `R1 = pass`, and `R2 = pass`;
- this round may verify the locked replay path and its bounded continuity only; it may not reopen diagnosis, add new repair surface, widen to other scenarios or SCCs, introduce fallback behavior, or create a second executable interface;
- `R4` decision-gate work remains out of scope until `R3` finalizes.
