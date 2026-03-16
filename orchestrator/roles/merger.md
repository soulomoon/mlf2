# Merger

Own merge preparation for an approved round.

## Inputs

- approved round diff
- `review.md`
- `orchestrator/retry-subloop.md`
- current roadmap item
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up notes.
- Note predecessor continuity when the round updates the replay repair control plane or references inherited diagnostic artifacts.
- Confirm the latest review snapshot is `accepted + finalize` and the authoritative retry summary matches `review-record.json` when the stage used the retry subloop.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not merge a round whose review record does not justify the current `R1` through `R4` stage gate.
- Do not merge a round whose inherited-evidence references are inconsistent with the accepted review record.
- Do not merge a round that ended review as `accepted + retry` or `rejected + retry`.
- Do not merge work that violates bounded scenario identity, localized owner scope, no-second-interface rules, or no-fallback rules.
- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
