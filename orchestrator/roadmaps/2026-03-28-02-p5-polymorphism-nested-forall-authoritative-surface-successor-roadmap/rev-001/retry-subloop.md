# Retry Subloop Contract

This file defines the live retry behavior for `contract_version: 2` rounds on
the bounded `P5 polymorphism-nested-forall authoritative-surface` successor
loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

The current roadmap and subject boundary come from:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- the authoritative `roadmap_dir/roadmap.md`

## Scope

- roadmap item `1` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `1`.
- roadmap item `2` may retry inside the same round.
- roadmap item `3` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `3`.
- roadmap item `4` is aggregate-only:
  - review may reject it and send the same round back to `plan`;
  - review may not emit `accepted + retry` for item `4`.

Parallel subagents are allowed inside one selected round only when the plan
states bounded independent sidecars, assigns disjoint write scopes, and
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

Every boundary-freeze / implementation / settlement / successor-gate review
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
- `accepted + retry` for item `3`
- `accepted + retry` for item `4`

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

## Budget Rules

- `max_attempts` is `100` for roadmap item `2`
- items `1`, `3`, and `4` are aggregate-only and therefore finalize or reject;
  they do not use `accepted + retry`

## Resume Rules

- preserve the same round id, branch, and worktree during retries
- resume the exact recorded `plan`, `implement`, or `review` stage when
  interrupted
- do not create a replacement round merely because a retry was interrupted

## Roadmap-Update Rule

After an accepted round finalizes and merges:

- the guider may mark the completed item done;
- the guider may refine later pending items while staying inside the active
  bounded `P5` authoritative-surface family; and
- the guider may not silently reopen `C1`, the same-lane pocket, or the exact
  settled `P1` packet, claim repo-level capability success, authorize cyclic
  search, second interfaces, fallback paths, or rollout without saying so
  explicitly.
