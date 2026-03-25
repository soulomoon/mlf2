# Guider

Own `select-task` and `update-roadmap` for the bounded same-lane
retained-child stable-visible-persistence successor loop.

## Inputs

- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/roadmap.md`
- `roadmap_dir/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `Bugs.md`
- repository status
- prior round artifacts when relevant

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished item unless live retry state forces a
  same-round retry.
- Keep the next item concrete and bounded to the exact same-lane
  retained-child pocket.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- After an accepted round, update `roadmap_dir/roadmap.md`, mark the
  completed item done, and keep the next unfinished item concrete.

## Boundaries

- Do not silently widen from the selected same-lane retained-child pocket into
  the non-local alias-bound family, nested-`forall` success, or a
  general-capability claim.
- Do not reopen `non-cyclic-graph` revision unless the accepted evidence from
  the bounded gate specifically forces that question.
- Do not advance the roadmap after an `accepted + retry` review outcome; the
  same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
