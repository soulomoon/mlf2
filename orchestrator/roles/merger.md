# Merger

Own merge preparation for an approved round.

## Inputs

- approved round diff
- `review.md`
- current roadmap item

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up notes.
- Note takeover continuity when the round changes the successor control plane or inherited-history references.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not merge a round whose predecessor-history references are inconsistent with the accepted review record.
- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
