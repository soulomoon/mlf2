# Round 027 Selection

Date: 2026-03-17
Round: `round-027`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
Controlling bug: `BUG-2026-03-16-001`

## Roadmap Provenance

- Roadmap ID: `2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap`
- Roadmap Revision: `rev-004`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004`
- State Snapshot: `orchestrator/rounds/round-027/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 4: execute the `R4` repair decision gate for `URI-R2-C1`.

## Why This Item Should Run Now

`orchestrator/rounds/round-027/state-snapshot.json` shows a fresh `select-task` transition for `round-027` with `current_task: null` and `retry: null`, so the retry subloop is idle and does not force a same-round return to `R1`, `R2`, or `R3`. Under the guider contract, selection therefore falls to the lowest-numbered unfinished roadmap item.

`orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/roadmap.md` marks `R1`, `R2`, and `R3` as `done`, leaving `R4` as the only remaining `pending` item. The last completed round, `round-026`, finalized `R3` attempt `1` as the authoritative verification pass for the locked `URI-R2-C1` / `uri-r2-c1-only-v1` replay lane, so the dependency chain for the terminal decision gate is now satisfied.

`R4` should therefore run now because the repair track has already completed the bounded reproduction (`R1`), bounded `InstBot` repair (`R2`), and locked replay-path verification (`R3`) for the localized `applyInstantiation` / `InstBot` boundary. The only remaining live work is to aggregate those accepted bounded results and record exactly one final outcome, `repair-accepted` or `repair-blocked`, without reopening diagnosis, widening scope, or changing the fixed `URI-R2-C1` scenario and repair target context.

## Round Scope Guard

This round is limited to roadmap item 4 only. It must stay inside the approved repair-track successor control plane and preserve the locked repair context.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- admissible repair target context remains `witness-replay/applyInstantiation-instbot-precondition` only;
- admissible owner boundary remains `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) only;
- controlling bug remains `BUG-2026-03-16-001`;
- inherited predecessor authority remains `P1 = pass`, `P2 = semantic-negative`, `D1 = pass`, `D2 = pass`, `D3 = pass`, `D4 = reopen-repair-track`, `R1 = pass`, `R2 = pass`, and `R3 = pass`;
- this round may aggregate and decide the bounded repair-track outcome only; it may not reopen earlier stages, add new repair surface, widen to other scenarios or SCCs, introduce fallback behavior, or create a second executable interface.
