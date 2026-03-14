# Round 013 Selection

Selected roadmap item: **3. Write the `RE3` prototype-free positive-evidence contract for `URI-R3-O1` through `URI-R3-O3`** (`orchestrator/roadmap.md`, item 3).

Why this item should run now:
- Item 3 is the next pending stage in the approved `RE1` -> `RE5` re-entry ladder, and its only roadmap dependency is item 2, which accepted round `012` completed via `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`.
- The accepted `R5` bounded `research-stop` explicitly leaves `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` unresolved because there is still no prototype-free positive evidence that `URI-R2-C1` can preserve structural acyclicity, deterministic single-binder-family ownership, and constructor-directed occurs-check or termination clearance without leaning on explicit anchors.
- Writing the `RE3` contract is the smallest remaining evidence slice that sharpens the path from the accepted bounded stop to a bounded re-entry verdict, because `RE4` cannot responsibly execute the re-entry gate until those positive-evidence requirements are made reviewer-checkable rather than deferred to the gate or smuggled in through prototype-backed claims.
- Choosing item 3 keeps `URI-R2-C1` fixed and keeps the mandatory boundaries explicit: `single-SCC` only, `single-binder-family` only, non-equi-recursive semantics only, and non-cyclic structural-graph encoding only. The round remains research-only, prototype-free, fail-closed, and does not reopen the handoff track.

Round-013 scope guard:
- Produce only the `RE3` prototype-free positive-evidence contract for `URI-R3-O1` through `URI-R3-O3`; do not pre-empt `RE4` or `RE5`.
- Keep the active subject fixed to `URI-R2-C1` only, with `single-SCC` obligation-level recursion, `single-binder-family` ownership, non-equi-recursive semantics, and non-cyclic structural-graph constraints explicit throughout.
- Treat completed rounds `001` through `012`, the accepted `R5` stop decision, the approved re-entry roadmap design, and the predecessor recursive-types packet as inherited evidence only; do not reopen or rewrite them.
- Keep the round prototype-free and fail-closed: if the positive-evidence contract cannot be stated without prototype-backed evidence, implementation drift, explicit-anchor replay substitution, widened ownership or search, cyclic graph reasoning, or equi-recursive assumptions, preserve the stop rather than authorizing such work.
- Do not write `plan.md`, and do not edit `orchestrator/roadmap.md`, `orchestrator/state.json`, production code, tests, docs outside the round deliverable, or predecessor packet files.
