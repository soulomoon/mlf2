# Round 012 Selection

Selected roadmap item: **2. Write the `RE2` uniqueness-evidence contract for `URI-R3-O5`** (`orchestrator/roadmap.md`, item 2).

Why this item should run now:
- Item 2 is the next pending stage in the approved `RE1` -> `RE5` re-entry ladder, and its only roadmap dependency is item 1, which accepted round `011` completed via `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`.
- The accepted `R5` bounded `research-stop` names `URI-R3-O5` as a separate unresolved blocker after provenance authority: there is still no bounded docs-only showing that `URI-R2-C1` contains one uniquely admissible local root or cluster without heuristic ranking, competing roots, or widening-dependent choice.
- Writing the `RE2` contract is the smallest remaining evidence slice that sharpens the path from the accepted stop to a bounded re-entry verdict, because `RE4` cannot responsibly judge re-entry until uniqueness is reviewer-checkable rather than implicitly folded into provenance or postponed to the gate.
- Choosing item 2 keeps `URI-R2-C1` fixed and keeps the mandatory boundaries explicit: `single-SCC` only, `single-binder-family` only, non-equi-recursive semantics only, and non-cyclic structural-graph encoding only. The round stays research-only, prototype-free, and fail-closed.

Round-012 scope guard:
- Produce only the `RE2` uniqueness-evidence contract for `URI-R3-O5`; do not pre-empt `RE3`, `RE4`, or `RE5`.
- Keep the active subject fixed to `URI-R2-C1` only, with `single-SCC` obligation-level recursion, `single-binder-family` ownership, non-equi-recursive semantics, and non-cyclic structural-graph constraints explicit throughout.
- Treat completed rounds `001` through `011`, the accepted `R5` stop decision, the approved re-entry roadmap design, and the predecessor recursive-types packet as inherited evidence only; do not reopen or rewrite them.
- Keep the round prototype-free and fail-closed: if the uniqueness contract cannot be stated without heuristic ranking, widened comparison, prototype-backed evidence, experiment-backed evidence, or implementation drift, preserve the stop rather than authorizing such work.
- Do not write `plan.md`, and do not edit `orchestrator/roadmap.md`, `orchestrator/state.json`, production code, tests, docs outside the round deliverable, or predecessor packet files.
