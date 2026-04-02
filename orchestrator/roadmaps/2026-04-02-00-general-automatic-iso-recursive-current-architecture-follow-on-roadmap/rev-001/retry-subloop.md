# Retry Subloop Contract

Roadmap family: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap`
Revision: `rev-001`

## Scope

- `item-1` retries for:
  stale or incomplete predecessor authority,
  an unfrozen next representative-gap packet,
  a missing exact live read,
  or an overbroad writable slice.
- `item-2` retries for:
  scope creep outside the frozen writable slice,
  unsupported semantic widening,
  missing focused verification,
  or a claimed result that is not backed by the diff and tests.
- `item-3` retries for:
  provenance gaps,
  settlement text that silently upgrades one packet into broader readiness,
  or an exact repo-impact read that does not match item-2 evidence.
- `item-4` retries for:
  mixed or ambiguous decision tokens,
  multiple handoff tokens,
  or any implicit boundary revision not explicitly earned by the evidence.

Review may reject and return the same round to `plan`.
Maximum 3 retry attempts per round before escalation.

## Machine State

`orchestrator/state.json` must carry:

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
  - controller records the attempt
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`

## Boundary Rules

- Do not widen from the frozen packet or aggregate decision into a different
  family without a later accepted roadmap update.
- Do not rewrite already-used predecessor roadmap revisions.
- Do not treat documentation-only settlement as implementation clearance.
- Do not treat one bounded packet as a broad repo-level capability claim.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must not continue automatically until a human or an explicit
  recovery recommendation reopens the round
