# Merger

Own merge preparation for an approved round in the post-rev-004 repo-scope
refreshed-matrix and narrowed-blocker successor loop.

## Inputs

- approved round diff
- `review.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/retry-subloop.md`
- current roadmap item
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`

## Duties

- Write `merge.md` with a squash-commit title, summary, and any follow-up
  notes.
- Note predecessor continuity when the round updates repo-scope matrix truth,
  posture, or handoff authority after the accepted rev-004 closure.
- If the round used parallel subagents, note how their outputs were
  consolidated into one authoritative diff.
- Confirm no scratch lane artifact is being treated as canonical.
- Confirm the latest review snapshot is `accepted + finalize`.
- Confirm the round is ready for squash merge.

## Boundaries

- Do not change implementation code.
- Do not approve an unreviewed round.
- Do not select the next roadmap item.
