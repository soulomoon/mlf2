# Round 031 Selection

Date: 2026-03-18
Round: `round-031`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: unannotated iso-recursive inference (bounded)

## Selected Roadmap Item

Roadmap item 4: execute `U4` constructor-directed / acyclicity / termination clearance for the live subject.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `stage: select-task` for `round-031` with `current_task: null` and `retry: null`, so there is no same-round retry forcing a return to an earlier stage.

`orchestrator/roadmap.md` records `U1`, `U2`, and `U3` as done (finalized in rounds `round-028`, `round-029`, and `round-030`) and marks `U4` as the lowest-numbered unfinished item. Under the guider contract, `U4` is therefore the next lawful bounded selection.

Predecessor evidence is aligned for this handoff: `round-028` finalized the repaired-subject bind (`U1`), `round-029` finalized authority clearance with bounded token `authority-narrowed` (`U2`), and `round-030` finalized uniqueness/owner analysis with bounded token `uniqueness-owner-stable-refuted` (`U3`). The successor roadmap design and live roadmap define `U4` as the next required clearance step to prove or refute admissibility under constructor-directed reasoning without equi-recursion, cyclic structural graphs, or weakened termination guarantees.

`Bugs.md` remains continuity context, but this selection does not reopen broad repair-track scope or authorize widening; it keeps the live subject fixed to repaired `URI-R2-C1` and proceeds with the bounded roadmap order.

## Round Scope Guard

- This round is limited to roadmap item `U4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; no widening beyond the repaired lane.
- Preserve inherited boundary constraints: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph encoding, and no hidden default-on widening.
- Target only the bounded `U4` artifact path named by the roadmap: `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`.
- Do not preempt `U5` or `U6`, and do not introduce a second executable interface, compatibility fallback, or convenience widening path.
