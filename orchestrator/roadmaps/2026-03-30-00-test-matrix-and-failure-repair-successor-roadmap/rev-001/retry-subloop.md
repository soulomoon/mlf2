# Retry Subloop Contract

This file defines retry behavior for the CI test-matrix and failure-repair
successor loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

## Scope

- Item `1` retries for incorrect scope decisions, incomplete baseline repair,
  or source-of-truth drift fixes that do not actually make the thesis gate
  green.
- Item `2` retries for workflow-shape mistakes, matrix-scope drift, or repo
  command mismatches.
- Item `3` retries for incorrect root-cause analysis, missing regression
  coverage, or fixes that still leave a matrix lane red.
- Item `4` retries for stale guidance, incomplete handoff, or missing runner
  limitation notes.
- Review may reject and return the same round to `plan`.
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
  or a new recovery recommendation
