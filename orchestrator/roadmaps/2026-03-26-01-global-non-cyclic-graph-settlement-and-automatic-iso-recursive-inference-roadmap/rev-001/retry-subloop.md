# Retry Subloop Contract

This file defines the live retry behavior for future `contract_version: 2`
rounds on the current global `non-cyclic-graph` settlement and automatic
iso-recursive inference control plane.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

The current roadmap and subject boundary come from:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
- the authoritative `roadmap_dir/roadmap.md`

## Scope

- roadmap items `1` through `4` may retry inside the same round.
- roadmap item `5` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `5`.
- roadmap items `6` and `7` may retry inside the same round.
- roadmap item `8` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `8`.

## Machine State

`orchestrator/state.json` must carry:

- `contract_version: 2`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `retry: null` when idle, or a retry object when the same round is looping

Retry object fields:

- `stage_id`
- `attempt`
- `max_attempts`
- `latest_accepted_attempt`
- `latest_accepted_result`
- `latest_attempt_verdict`
- `latest_stage_action`
- `retry_reason`
- `fix_hypothesis`

## Review Output

Every settlement / implementation / hardening / capability review must
record:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed combinations:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

Forbidden combinations:

- `rejected + finalize`
- `accepted + retry` for item `5`
- `accepted + retry` for item `8`

Use `Retry reason: none` and `Fix hypothesis: none` when a stage finalizes
without another retry.

## Artifact Ownership

- reviewer owns `review.md`
- reviewer owns `reviews/attempt-<n>.md`
- reviewer owns `review-record.json` only on finalization
- controller owns `attempt-log.jsonl`
- planner, implementer, reviewer, and merger must not rewrite prior attempts

## Transition Rules

After review:

- `accepted + finalize`
  - controller clears `retry`
  - controller advances to `merge`
- `accepted + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller updates `latest_accepted_*`
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`
- `rejected + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller leaves `latest_accepted_*` unchanged
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`

## Budget Rules

- `max_attempts` is `100` for roadmap items `1` through `4`, `6`, and `7`
- on exhaustion:
  - finalize the latest accepted attempt if one exists
  - otherwise stop with a controller blockage in `orchestrator/state.json`

## Resume Rules

- preserve the same round id, branch, and worktree during retries
- resume the exact recorded `plan`, `implement`, or `review` stage when
  interrupted
- do not create a replacement round merely because a retry was interrupted

## Roadmap-Update Rule

After an accepted round finalizes and merges:

- the guider may mark the completed item done;
- the guider may refine later pending items or append the next bounded
  follow-on gate while staying inside the same roadmap family;
- the guider may publish a later roadmap revision under the same
  `roadmap_id` if accepted item `5` records
  `reopen the non-cyclic-graph revision question` or if accepted settlement
  evidence requires a concrete retune of later implementation / hardening
  items; and
- the guider may not rewrite completed-item truth or silently widen the live
  subject into cyclic search, multi-SCC search, second interfaces, fallback
  paths, or a broader capability claim without saying so.

## Historical Compatibility

Rounds `round-001` through `round-098` remain historical predecessor
evidence. New roadmap-family scaffolding may advance active roadmap metadata,
but it must not reinterpret accepted stage results into global settlement,
broader capability, or architecture claims that the accepted record does not
actually support.
