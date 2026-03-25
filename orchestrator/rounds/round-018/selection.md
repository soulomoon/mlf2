# Round 018 Selection

Date: 2026-03-15
Round: `round-018`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`

## Roadmap Provenance

- Roadmap ID: `2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap`
- Roadmap Revision: `rev-003`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-003`
- State Snapshot: `orchestrator/rounds/round-018/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 3: execute `P3` safety-validation prototype for `URI-R2-C1`.

## Why This Item Should Run Now

`P1` is already complete and authoritative at attempt 2 in `orchestrator/rounds/round-016/review-record.json`, and `P2` is now complete and authoritative at attempt 2 in `orchestrator/rounds/round-017/review-record.json` with result `semantic-negative`. Item 3 is therefore the lowest-numbered unfinished roadmap item, and no prior review artifact forces a same-round retry of `P1` or `P2`.

Running `P3` now keeps the successor track honest about the next bounded gate. The design still requires an explicit `P3` stage result before `P4` can consume the full `P1`/`P2`/`P3` evidence vector, but `P3` must not manufacture a replacement authority after the accepted `P2` non-pass. The round should therefore test safety only inside the same bounded `URI-R2-C1` / `uri-r2-c1-only-v1` lane, using the authoritative inherited subject lane from `P1` and the accepted `P2` outcome as fixed constraints, and it must record any inability to establish bounded safety as an explicit bounded result rather than a widened workaround.

## Round Scope Guard

This round is limited to roadmap item 3 only. It must remain inside the approved `URI-R2-C1` prototype-evidence track and must not rewrite, relax, or supersede the accepted prototype-free `RE1` through `RE5` record or the reviewer-authoritative `P1` and `P2` results.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- single-SCC scope only;
- single-binder-family ownership only;
- non-equi-recursive reasoning only;
- non-cyclic structural-graph discipline only;
- shared-entrypoint isolation only, through `uri-r2-c1-prototype-entrypoint-v1`;
- no second executable interface, no widened comparison class, no widened search scope, no cross-family SCC linking, and no default production-behavior change;
- no surrogate token, repaired authority, or other replacement for the reviewer-authoritative `P1` subject lane and accepted `P2` result.
