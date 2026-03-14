# Unannotated Iso-Recursive Inference Successor Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed automatic-recursive-inference control plane whose accepted execution record lives in `orchestrator/rounds/round-001` through `round-005`.
- The completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains immutable predecessor evidence.
- The approved design source for this successor track is `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`.
- Scope stays research-first: the target is a research-backed implementation handoff for unannotated, single-SCC, single-binder-family iso-recursive inference, not a production implementation milestone.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [pending] Write the `R1` gap map from `ARI-C1` to unannotated single-SCC, single-binder-family inference
   Depends on: completed automatic-recursive-inference rounds `001` through `005`, plus the approved successor design spec
   Completion notes: not started; this item should enumerate what information currently comes only from explicit anchors, what may be recoverable locally without widening semantics, and which unannotated cases remain blocked while preserving the accepted item-2 invariant audit.

2. [pending] Select exactly one bounded unannotated candidate subset and admissibility contract
   Depends on: item 1
   Completion notes: not started; this item must choose one stable candidate identifier, keep the single-binder-family / no-cross-family-linking boundary explicit, and record deferred or rejected alternatives.

3. [pending] Write the `R3` inference-obligation contract for the chosen subset
   Depends on: item 2
   Completion notes: not started; this item must carry forward acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality obligations for the chosen subset.

4. [pending] Execute the bounded feasibility decision for the chosen subset
   Depends on: item 3
   Completion notes: not started; this item must record explicit `feasible-continue` or `not-yet-go` evidence for the chosen subset without equi-recursive, cyclic-graph, multi-SCC, or cross-family widening. Any prototype evidence must stay bounded and non-default.

5. [pending] Write the final implementation-handoff spec or explicit research-stop decision
   Depends on: item 4
   Completion notes: not started; if item 4 resolves to `feasible-continue`, write the bounded implementation handoff for the unannotated target, otherwise record the `not-yet-go` stop with explicit reasons.
