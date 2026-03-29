# Retry Subloop Contract

This file defines the live retry behavior for `contract_version: 2` rounds on
the iso-recursive inference gap-fixes loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

## Scope

- Items 1–4 (implementation rounds) may retry inside the same round when a
  reviewer rejects with fixable issues. These are complex Haskell changes
  touching the constraint solver, scheme finalization, reduction, and
  result-type reconstruction — expect higher retry rates than documentation
  rounds.
- Item 5 (docs-only) retries only for formatting or content defects.
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

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller sets `controller_stage: "blocked"`
- controller records the failure summary in `resume_error`
- the orchestrator must not proceed until the controller receives human input
  or an oracle consultation resolves the blocking issue
