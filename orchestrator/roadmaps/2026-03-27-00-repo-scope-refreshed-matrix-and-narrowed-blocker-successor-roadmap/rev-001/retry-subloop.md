# Retry Subloop Contract

This file defines the live retry behavior for future `contract_version: 2`
rounds on the post-rev-004 repo-scope representative-matrix refresh and
narrowed successor-gate loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

The current roadmap and subject boundary come from:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`
- the authoritative `roadmap_dir/roadmap.md`

## Scope

- roadmap item `1` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `1`.
- roadmap item `2` may retry inside the same round.
- roadmap item `3` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `3`.

Parallel subagents are allowed inside a single selected round only when the
plan states bounded independent sub-slices, assigns disjoint write scopes, and
preserves one authoritative round-owned output set.

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

Every boundary-freeze / refreshed-matrix / successor-gate review must record:

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
- `accepted + retry` for item `3`

Use `Retry reason: none` and `Fix hypothesis: none` when a stage finalizes
without another retry.

## Artifact Ownership

- reviewer owns `review.md`
- reviewer owns `reviews/attempt-<n>.md`
- reviewer owns `review-record.json` only on finalization
- controller owns `attempt-log.jsonl`
- planner, implementer, reviewer, and merger must not rewrite prior attempts
- when parallel subagents are used inside one round, the plan must name the
  authoritative owner for each writable file and keep write scopes disjoint
- lane-local scratch artifacts may exist on disjoint filenames, but
  `selection.md`, `plan.md`, authoritative docs, `implementation-notes.md`,
  `review.md`, `review-record.json`, and `merge.md` remain single-writer

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

When a retry follows parallel work, the next plan must freeze successful lanes
and target only failed lanes or synthesis defects.

## Budget Rules

- `max_attempts` is `100` for roadmap item `2`
- items `1` and `3` are aggregate-only and therefore finalize or reject;
  they do not use `accepted + retry`
- on exhaustion for retry-capable items:
  - finalize the latest accepted attempt if one exists
  - otherwise stop with a controller blockage in `orchestrator/state.json`

## Resume Rules

- preserve the same round id, branch, and worktree during retries
- resume the exact recorded `plan`, `implement`, or `review` stage when
  interrupted
- do not create a replacement round merely because a retry was interrupted
- if a parallel subagent path fails during a retry-capable round, the next
  plan must narrow to the recorded `fix_hypothesis`, preserve successful lanes
  as frozen, and avoid respawning the whole round without diagnosis

## Roadmap-Update Rule

After an accepted round finalizes and merges:

- the guider may mark the completed item done;
- the guider may refine later pending items while staying inside the active
  post-rev-004 repo-scope refreshed-matrix and narrowed-blocker family;
- the guider may publish a later roadmap revision under the same
  `roadmap_id` if item `3` chooses stop or a bounded current-architecture
  continuation; and
- the guider may not silently reopen the settled same-lane pocket, claim
  repo-level capability success, authorize cyclic search, second interfaces,
  fallback paths, or rollout without saying so explicitly.
