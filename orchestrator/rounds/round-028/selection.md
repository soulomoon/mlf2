# Round 028 Selection

Date: 2026-03-18
Round: `round-028`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: unannotated iso-recursive inference (bounded)

## Roadmap Provenance

- Roadmap ID: `2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap`
- Roadmap Revision: `rev-006`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-006`
- State Snapshot: `orchestrator/rounds/round-028/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 1: execute `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference.

## Why This Item Should Run Now

`orchestrator/rounds/round-028/state-snapshot.json` is parked at `stage: select-task` for `round-028` with `current_task: null` and `retry: null`, so no same-round retry is active and no earlier stage is forced.

`orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-006/roadmap.md` lists the successor cycle as `U1` through `U6`, all pending; under the guider contract, the next lawful selection is the lowest-numbered unfinished item, `U1`.

Predecessor evidence is already finalized for entry into this successor cycle: `round-027` finalized `R4` as `accepted + finalize` with final outcome `repair-accepted`, and the terminal artifact `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` records that repaired `URI-R2-C1` is the controlling bounded subject for successor work. The approved successor design (`docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`) also explicitly starts new live rounds at `round-028` with `U1` as the first concrete step.

`U1` should therefore run now because it is the required bind step that restates inherited baseline truth and hard-stop constraints before any authority/uniqueness/feasibility or implementation slice stages (`U2`-`U6`) can proceed.

## Round Scope Guard

- This round is limited to roadmap item `U1` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen to broad automatic recursive inference or other subjects.
- Keep the inherited hard-stop boundary reviewer-visible: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph, no hidden default-on widening.
- Target only the bounded `U1` artifact path named by the roadmap: `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`.
- Do not preempt `U2` through `U6`, and do not introduce a second executable interface, compatibility fallback, or convenience widening path.
