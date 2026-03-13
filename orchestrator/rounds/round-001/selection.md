# Round 001 Selection

Selected roadmap item: **1. Write the inherited-baseline and acceptance contract for automatic recursive-type inference** (`orchestrator/roadmap.md`, item 1).

Why this item now:
- The successor run baseline is already fixed: explicit-only, acyclic `TyMu` support is complete, while automatic recursive-type inference is unresolved.
- Item 1 is the required first dependency for every later research step and is explicitly the first pending roadmap action.
- A contract-first round keeps the loop research-first, preserves the explicit-only / non-equi-recursive boundary, and avoids prematurely selecting solver-behavior implementation or inference spikes.

Takeover continuity:
- Treat `tasks/todo/2026-03-11-recursive-types-orchestration/` as historical authority for the completed predecessor campaign and do not rewrite its milestone truth.
- Reuse the evidence already captured in:
  - `docs/plans/2026-03-11-recursive-types-roadmap.md`
  - `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`
  - `docs/plans/2026-03-13-m7-tymu-design-resolution.md`
- Produce only the inherited-baseline + acceptance-contract artifact for automatic recursive-type inference; do not start implementation planning or code changes in this round.
