# Round 003 Selection

Selected roadmap item: **3. Select one bounded candidate subset and research plan** (`orchestrator/roadmap.md`, item 3).

Why this item now:
- Items 1 and 2 are already complete, and item 3 is the next dependency before any bounded feasibility spike (item 4).
- This is the smallest research slice that increases confidence now: choose exactly one candidate subset and define verifier-visible pass/fail gates, without changing solver behavior.
- The candidate selection can be constrained to explicit-annotation-anchored recursion paths only, which keeps automatic recursive-type inference unresolved while still testing whether a narrow subset is even worth a spike.
- It preserves the mandatory boundary: explicit-only behavior, non-equi-recursive semantics, and non-cyclic graph representation.

Round-003 scope guard:
- Produce only the item-3 selection artifact (chosen subset, deferred/rejected alternatives, and spike gates).
- Do not write `plan.md`, do not edit `orchestrator/roadmap.md`, and do not edit production code.
