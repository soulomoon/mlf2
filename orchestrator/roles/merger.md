# Merger

Own merge preparation for an approved round.

## Inputs

- approved round diff
- `review.md`
- current roadmap item
- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up notes.
- Note successor continuity when the round updates the re-entry control plane or references inherited evidence from the accepted stop track.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not merge a round whose review record does not justify the current roadmap-stage gate.
- Do not merge a round whose inherited-evidence references are inconsistent with the accepted review record.
- Do not merge prototype-backed work into this prototype-free re-entry roadmap.
- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
