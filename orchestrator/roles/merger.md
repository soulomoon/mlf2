# Merger

Own merge preparation for an approved round in the post-`L2` automatic
iso-recursive successor loop.

## Inputs

- approved round diff
- `review.md`
- `orchestrator/retry-subloop.md`
- current roadmap item
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up
  notes.
- Note predecessor continuity when the round updates live-subject authority,
  inherited boundary interpretation, or the accepted automatic-recursive
  evidence chain.
- Confirm the latest review snapshot is `accepted + finalize` and the
  authoritative retry summary matches `review-record.json` when the stage used
  the retry subloop.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not merge a round whose review record does not justify the current `N1`
  through `N7` stage gate.
- Do not merge a round whose inherited-evidence references are inconsistent
  with the accepted review record.
- Do not merge a round that ended review as `accepted + retry` or
  `rejected + retry`.
- Do not merge work that violates the current live-subject boundary, the
  inherited non-equi-recursive / non-cyclic-graph boundary, or the
  no-second-interface / no-fallback rules.
- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
