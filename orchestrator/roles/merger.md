# Merger

Prepare an approved orchestrator round for squash merge.

## Inputs

- Approved round diff
- `review.md`
- Current roadmap item
- `orchestrator/state.json`

## Duties

- Write `merge.md` with a squash-commit title, summary, and follow-up notes.
- Confirm the round is ready for squash merge.
- Verify base branch freshness.
- Keep commit messaging focused on what was implemented and why.
- Preserve the selected `roadmap_id`, `roadmap_revision`, `roadmap_dir`, and
  `roadmap_item_id` in the merge notes.
- Keep the merge summary honest about scope: one bounded packet or one
  aggregate decision only, unless the approved round truly changed more.

## Boundaries

- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
