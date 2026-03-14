# Round 017 Selection

Date: 2026-03-15
Round: `round-017`
Role: guider
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`

## Selected Roadmap Item

Roadmap item 2: execute `P2` provenance-preservation prototype for `URI-R2-C1`.

## Why This Item Should Run Now

`P1` is already complete and authoritative at attempt 2 in `orchestrator/rounds/round-016/review-record.json`, with result `pass` and a canonical subject token emitted for `uri-r2-c1-only-v1`. The roadmap therefore leaves item 2 as the lowest-numbered unfinished item, and there is no prior review rejection for `round-017` that would force a same-round retry instead of advancement.

Running `P2` now is the next bounded step because the prototype-evidence roadmap requires the exact `P1` canonical subject to be carried through `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback` and witness replay before any safety-validation or final decision stage can be meaningfully judged. This preserves the accepted `RE4` `not-yet-reopen` and `RE5` `remain-stop` authority while testing only the newly admitted prototype lane.

## Round Scope Guard

This round is limited to roadmap item 2 only. It must remain inside the approved `URI-R2-C1` prototype-evidence track and must not rewrite, relax, or supersede the accepted prototype-free `RE1` through `RE5` record.

Applicable fixed boundaries for this round:

- active subject remains `URI-R2-C1` only;
- active scenario remains `uri-r2-c1-only-v1` only;
- single-SCC scope only;
- single-binder-family ownership only;
- non-equi-recursive reasoning only;
- non-cyclic structural-graph discipline only;
- shared-entrypoint isolation only, through `uri-r2-c1-prototype-entrypoint-v1`;
- no second executable interface, no widened comparison class, no widened search scope, no cross-family SCC linking, and no default production-behavior change.
