# Retry Subloop Contract

This file defines the live retry behavior for future `contract_version: 2` rounds on the current unannotated iso-recursive successor control plane.

The current roadmap and subject boundary come from:

- `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`

## Scope

- `U1`, `U2`, `U3`, `U4`, and `U5` may retry inside the same round.
- `U6` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for `U6`.

## Machine State

`orchestrator/state.json` must carry:

- `contract_version: 2`
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

Every `U1` through `U6` review must record:

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
- `accepted + retry` for `U6`

Use `Retry reason: none` and `Fix hypothesis: none` when a stage finalizes without another retry.

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

- `max_attempts` is `100` for `U1`, `U2`, `U3`, `U4`, and `U5`
- on exhaustion:
  - finalize the latest accepted attempt if one exists
  - otherwise stop with a controller blockage in `orchestrator/state.json`

## Resume Rules

- preserve the same round id, branch, and worktree during retries
- resume the exact recorded `plan`, `implement`, or `review` stage when interrupted
- do not create a replacement round merely because a retry was interrupted

## Roadmap-Update Rule

After an accepted round finalizes and merges:

- the guider may mark the completed item done;
- the guider may refine later pending items or append the next bounded cycle;
- the guider may not rewrite completed-item truth or silently widen the active subject.

## Historical Compatibility

Rounds `round-001` through `round-027` remain historical predecessor evidence.
Do not rewrite them into the new retry schema.
