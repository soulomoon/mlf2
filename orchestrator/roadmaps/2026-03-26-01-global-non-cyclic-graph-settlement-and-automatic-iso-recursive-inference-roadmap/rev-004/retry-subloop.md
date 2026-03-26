# Retry Subloop Contract

This file defines the live retry behavior for future `contract_version: 2`
rounds on the bounded same-pocket post-amendment settlement lane inside the
current global `non-cyclic-graph` settlement and automatic iso-recursive
inference roadmap.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

The current roadmap and subject boundary come from:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-architecture-amendment-contract-and-writable-slice-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-handoff-decision.md`
- the authoritative `roadmap_dir/roadmap.md`

## Scope

- roadmap item `1` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `1`.
- roadmap items `2` and `3` may retry inside the same round.
- roadmap item `4` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `4`.

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

Every lane-freeze / docs-settlement / validation / follow-on-decision review
must record:

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
- `accepted + retry` for item `1`
- `accepted + retry` for item `4`

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

- `max_attempts` is `100` for roadmap items `2` and `3`
- item `1` and item `4` are aggregate-only and therefore finalize or reject;
  they do not use `accepted + retry`
- on exhaustion for retry-capable items:
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
- the guider may refine later pending items while staying inside the same
  exact-pocket post-amendment settlement lane;
- the guider may publish a later roadmap revision under the same
  `roadmap_id` if accepted item `4` records either a bounded same-family
  successor or a stop result; and
- the guider may not silently widen into code implementation, second
  interfaces, multi-SCC search, fallback paths, rollout, or broad capability
  claims without saying so explicitly.
