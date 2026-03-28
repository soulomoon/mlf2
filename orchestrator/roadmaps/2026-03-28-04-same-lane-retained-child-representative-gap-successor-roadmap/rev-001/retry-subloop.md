# Retry Subloop Contract

This file defines the live retry behavior for `contract_version: 2` rounds on
the same-lane retained-child representative-gap successor loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

## Scope

- roadmap item `2` may retry inside the same round
- roadmap items `1`, `3`, and `4` are aggregate-only
- review may reject an item and send the same round back to `plan`
- review may not emit `accepted + retry` for items `1`, `3`, or `4`

## Machine State

`orchestrator/state.json` must carry:

- `contract_version: 2`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `retry: null` when idle, or a retry object when the same round is looping

## Review Output

Every review must record:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed combinations:

- `accepted + finalize`
- `accepted + retry` for item `2`
- `rejected + retry`

Forbidden combinations:

- `rejected + finalize`
- `accepted + retry` for items `1`, `3`, or `4`

## Transition Rules

After review:

- `accepted + finalize`
  - controller clears `retry`
  - controller advances to `merge`
- `accepted + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`
- `rejected + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`
