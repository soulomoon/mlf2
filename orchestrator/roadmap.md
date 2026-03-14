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

1. [done] Write the `R1` gap map from `ARI-C1` to unannotated single-SCC, single-binder-family inference
   Depends on: completed automatic-recursive-inference rounds `001` through `005`, plus the approved successor design spec
   Completion notes: completed in accepted `round-006` via `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`; the accepted `R1` artifact maps the finite delta from `R0`/`ARI-C1` by separating explicit-anchor inputs, potentially locally recoverable facts, and still-blocked unannotated cases while keeping the fixed boundary explicit: single-SCC only, single-binder-family only, obligation-level recursion only, non-cyclic structural graph only, non-equi-recursive only, and no default-on widening.

2. [done] Select exactly one bounded unannotated candidate subset and admissibility contract
   Depends on: item 1
   Completion notes: completed in accepted `round-007` via `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`; the accepted `R2` result selects candidate `URI-R2-C1` ("Unique local obligation root with one closed binder-family cluster") and keeps the fixed boundary explicit: single-SCC obligation-level recursion only, single-binder-family ownership only, no cross-family SCC linking, non-equi-recursive semantics only, non-cyclic structural graph only, and fail-closed rejection of ambiguous or widening-dependent cases.

3. [done] Write the `R3` inference-obligation contract for the chosen subset
   Depends on: item 2
   Completion notes: completed in accepted `round-008` via `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`; the accepted `R3` result fixes the `URI-R2-C1` obligation contract across structural acyclicity, binder ownership/scope discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries while keeping the fixed boundary explicit: single-SCC obligation-level recursion only, single-binder-family ownership only, no cross-family SCC linking, non-equi-recursive semantics only, non-cyclic structural graph only, and fail-closed rejection of ambiguous or widening-dependent cases.

4. [done] Execute the bounded feasibility decision for the chosen subset
   Depends on: item 3
   Completion notes: completed in accepted `round-009` via `docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`; the accepted `R4` result records the bounded feasibility outcome `not-yet-go` for `URI-R2-C1` while keeping the fixed boundary explicit: `single-SCC` obligation-level recursion only, `single-binder-family` ownership only, no cross-family SCC linking, non-equi-recursive semantics only, non-cyclic structural graph only, docs-only inherited evidence only, and no default-on widening or prototype-backed expansion.

5. [done] Write the final implementation-handoff spec or explicit research-stop decision
   Depends on: item 4
   Completion notes: completed in accepted `round-010` via `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`; the accepted `R5` result takes the explicit `research-stop` branch for `URI-R2-C1`, carrying forward the accepted `R4` outcome `not-yet-go` and recording the bounded reasons to stop without widening beyond the fixed single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph boundary.
