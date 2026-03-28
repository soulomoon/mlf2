# Retry Subloop Contract

This file defines the live retry behavior for `contract_version: 2` rounds on
the automatic iso-recursive type inference completion loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

## Scope

- `item-1` (end-to-end validation) may retry inside the same round when a
  reviewer rejects with fixable issues.
- `item-2` (docs-only) retries only for formatting or content defects.
- `item-3` (cleanup/readiness) retries only for incomplete cleanup or missing
  readiness evidence.
- Review may reject and send the same round back to `plan`.
- Maximum 3 retry attempts per round before escalation.

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
- `rejected + retry`

Forbidden combinations:

- `rejected + finalize`

## Transition Rules

After review:

- `accepted + finalize`
  - controller clears `retry`
  - controller advances to `merge`
- `rejected + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`
