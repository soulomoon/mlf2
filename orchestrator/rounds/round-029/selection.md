# Round 029 Selection

Date: 2026-03-18
Round: `round-029`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: unannotated iso-recursive inference (bounded)

## Roadmap Provenance

- Roadmap ID: `2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap`
- Roadmap Revision: `rev-007`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007`
- State Snapshot: `orchestrator/rounds/round-029/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 2: execute `U2` provenance-stable unannotated authority clearance for the live subject.

## Why This Item Should Run Now

`orchestrator/rounds/round-029/state-snapshot.json` is parked at `stage: select-task` for `round-029` with `current_task: null` and `retry: null`, so no same-round retry is active and no earlier stage is forced.

`orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/roadmap.md` records `U1` as done (finalized in `round-028`) and `U2` as the lowest-numbered unfinished item. Under the guider contract, this makes `U2` the next lawful selection.

Predecessor and carry-forward evidence also make `U2` the correct next bounded step: `round-028` finalized `U1` as `accepted + finalize` (`orchestrator/rounds/round-028/review-record.json`), which bound the successor work to repaired `URI-R2-C1` and preserved hard stops against broad widening. The inherited `R5` research-stop decision identifies unresolved provenance-stable unannotated authority as the decisive blocker; `U2` is the roadmap item explicitly scoped to clear or narrowly retain that blocker for the same live subject.

## Round Scope Guard

- This round is limited to roadmap item `U2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; no widening beyond the current bounded subject.
- Keep inherited boundary constraints intact: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph, and no hidden default-on widening.
- Target only the bounded `U2` artifact path named by the roadmap: `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`.
- Do not preempt `U3` through `U6`, and do not introduce a second executable interface, compatibility fallback, or convenience widening path.
