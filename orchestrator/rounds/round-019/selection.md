# Round 019 Selection

Date: 2026-03-15
Round: `round-019`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`

## Selected Roadmap Item

Roadmap item 4: execute the `P4` prototype decision gate for `URI-R2-C1`.

## Why This Item Should Run Now

`P1`, `P2`, and `P3` are already complete and reviewer-authoritative in `orchestrator/rounds/round-016/review-record.json`, `orchestrator/rounds/round-017/review-record.json`, and `orchestrator/rounds/round-018/review-record.json`. Item 4 is therefore the only unfinished roadmap item, and no prior review artifact forces a same-round retry of an earlier stage.

Running `P4` now is the required terminal gate for the bounded prototype-evidence track. The approved design says `P4` must consume the actual authoritative `P1` through `P3` results and emit exactly one terminal decision: `reopen-handoff-track` or `hard-stop`. Given that authoritative `P2` is `semantic-negative` and authoritative `P3` is `semantic-negative`, the final gate must stay honest about those bounded negative results instead of reopening earlier stages, widening the subject lane, or manufacturing a positive implementation handoff.

## Round Scope Guard

This round is limited to roadmap item 4 only. It must remain inside the approved `URI-R2-C1` prototype-evidence track and must not rewrite, relax, or supersede the accepted prototype-free `RE1` through `RE5` record or the reviewer-authoritative `P1`, `P2`, and `P3` results.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- single-SCC scope only;
- single-binder-family ownership only;
- non-equi-recursive reasoning only;
- non-cyclic structural-graph discipline only;
- shared-entrypoint isolation only, through `uri-r2-c1-prototype-entrypoint-v1`;
- no second executable interface, no widened comparison class, no widened search scope, no cross-family SCC linking, and no default production-behavior change;
- no reopening of `P1`, `P2`, or `P3` authority except through explicit `P4` consumption of their accepted review records;
- final output must be exactly one bounded terminal decision for this subject: `reopen-handoff-track` or `hard-stop`.
