# Round 002 Selection

Selected roadmap item: **2. Audit thesis and solver invariants threatened by automatic recursive-type inference** (`orchestrator/roadmap.md`, item 2).

Why this item now:
- Item 2 is the immediate dependency after completed item 1 and must be finished before any candidate-subset selection (item 3).
- It is the smallest high-value research slice: document concrete risk boundaries (acyclicity, binding/tree discipline, occurs-check/termination, reconstruction, principality) without changing solver behavior.
- It directly increases confidence about automatic recursive-type inference by turning the current “unresolved” status into verifier-checkable obligations.
- It preserves the mandatory boundary: explicit-only behavior remains in force, no equi-recursive semantics, and no cyclic graph encoding.

Scope guard for this round:
- Produce an audit artifact only; no implementation planning, no production-code edits, and no roadmap mutation in this step.
