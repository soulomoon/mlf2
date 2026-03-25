# Round 020 Selection

Date: 2026-03-16
Round: `round-020`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Research entrypoint: `uri-r2-c1-p2-replay-root-cause-v1`

## Roadmap Provenance

- Roadmap ID: `2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001`
- State Snapshot: `orchestrator/rounds/round-020/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 1: execute the `D1` replay-failure reproduction contract for `URI-R2-C1`.

## Why This Item Should Run Now

`round-020` begins the successor replay root-cause roadmap, and the live roadmap still shows `D1` through `D4` as pending with no active retry state recorded in `orchestrator/rounds/round-020/state-snapshot.json`. Under the guider contract, the next round must therefore select the lowest-numbered unfinished item unless a same-round retry is already in force. No such retry exists here.

`D1` must run now because the approved successor design requires the new track to anchor itself on the real inherited failure before any localization or fixability claim is lawful. The controlling prior record is already bounded: `P1` attempt 2 is the authoritative subject source, `P2` attempt 2 is the accepted replay-failure artifact, and the accepted mismatch boundary is the `P2-W` `partial-replay` diagnostic with `InstBot expects ⊥, got: t9 -> t9`. Reproducing that exact bounded failure, or recording a bounded inability to do so without widening, is the prerequisite for `D2` localization and any later `D3` fixability probe.

## Round Scope Guard

This round is limited to roadmap item 1 only. It must remain inside the approved `URI-R2-C1` replay root-cause track and must not reopen the completed `P1` through `P4` prototype-evidence roadmap as unfinished work.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- inherited authoritative subject input remains the `P1` attempt 2 subject token only;
- inherited replay-failure boundary remains the accepted `P2-W` `partial-replay` mismatch only;
- investigation stays inside the bounded `P2` replay lane and its direct prerequisites only;
- no widened subject search, alternate subset, surrogate subject, or cross-scenario comparison is admissible;
- no production implementation milestone, no default production-behavior change, and no second executable interface is admissible;
- downstream `D2`, `D3`, and `D4` claims are out of scope until `D1` finalizes.
