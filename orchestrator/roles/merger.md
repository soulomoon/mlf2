# Merger

Own merge preparation for an approved round in the continue-bounded follow-on cycle.

## Inputs

- approved round diff
- `review.md`
- `orchestrator/retry-subloop.md`
- current roadmap item
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up notes.
- Note predecessor continuity when the round updates the follow-on control plane, the live subject, or the inherited recursive-inference evidence chain.
- Confirm the latest review snapshot is `accepted + finalize` and the authoritative retry summary matches `review-record.json` when the stage used the retry subloop.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not merge a round whose review record does not justify the current `C1` through `C4` stage gate.
- Do not merge a round whose inherited-evidence references are inconsistent with the accepted review record.
- Do not merge a round that ended review as `accepted + retry` or `rejected + retry`.
- Do not merge work that violates the current live-subject boundary, the inherited non-equi-recursive / non-cyclic-graph boundary, or the no-second-interface / no-fallback rules.
- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
