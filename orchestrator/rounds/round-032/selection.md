# Round 032 Selection

Date: 2026-03-18
Round: `round-032`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: unannotated iso-recursive inference (bounded)

## Selected Roadmap Item

Roadmap item 5: execute `U5` bounded solver/pipeline implementation slice for the still-bound live subject under the `U4` refuted result.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `stage: select-task` for `round-032` with `current_task: null` and `retry: null`, so no same-round retry is active and no earlier stage is forced ahead of normal roadmap selection.

`orchestrator/roadmap.md` records `U1` through `U4` as done and marks `U5` as the lowest-numbered unfinished item. Under the guider contract, that makes `U5` the next lawful bounded selection.

The predecessor evidence is aligned for this handoff. `round-028`, `round-029`, `round-030`, and `round-031` each finalized cleanly (`accepted + finalize`) for `U1`, `U2`, `U3`, and `U4`, respectively. In particular, `round-031` finalized `U4` with the bounded result token `constructor-acyclic-termination-refuted`, and both the accepted `U4` artifact (`docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`) and the live roadmap now explicitly carry forward that `U5` must run on the still-bound repaired `URI-R2-C1` subject without treating `U4` as clearance for broader recursive behavior.

The approved successor-roadmap design also places `U5` after the evidence-clearance items and defines it as exactly one bounded solver/pipeline implementation slice, not a broad implementation push. `Bugs.md` remains continuity context, but this selection does not reopen broad repair-track scope or authorize subject widening; it moves to the next bounded roadmap item only.

## Round Scope Guard

- This round is limited to roadmap item `U5` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; no widening beyond the still-bound repaired lane.
- Preserve inherited boundary constraints: explicit-only baseline, non-equi-recursive semantics, and non-cyclic structural graph encoding.
- Treat the accepted `U4` refuted result as a fail-closed boundary, not as clearance for equi-recursive reasoning, cyclic graphs, fallback behavior, or cross-subject implementation.
- Target only the bounded `U5` artifact path named by the roadmap: `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`.
- Land exactly one bounded solver/pipeline slice, with no second executable interface, compatibility fallback, convenience widening path, or preemption of `U6`.
