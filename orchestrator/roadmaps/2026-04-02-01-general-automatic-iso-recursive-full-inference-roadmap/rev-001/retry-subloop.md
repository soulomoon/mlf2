# Retry Subloop Contract

Roadmap family: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
Revision: `rev-001`

## Scope

- `item-1` retries for:
  stale or incomplete predecessor authority,
  an unfrozen semantic family matrix,
  a missing family success bar,
  or an overbroad writable slice.
- `item-2` retries for:
  a mechanism map that collapses packet truth into unwarranted generality,
  missing separation between settled fragments and missing rules,
  or hidden architecture widening.
- `item-3` retries for:
  guessed candidate selection,
  unclear ambiguity rejection,
  missing soundness guards,
  or boundedness claims that are not actually explained.
- `item-4` retries for:
  ambiguous authoritative surfaces,
  a reconstruction contract that treats solver-only success as sufficient,
  or corpus obligations that are too vague to review.
- `item-5` retries for:
  scope creep outside the authorized writable surfaces,
  unsupported semantic widening,
  missing representative positive-family evidence,
  or claimed support not backed by the diff and tests.
- `item-6` retries for:
  missing ambiguity / soundness / termination evidence,
  unbounded search behavior,
  or negative cases misclassified as positive success.
- `item-7` retries for:
  mixed or ambiguous end-state outcomes,
  missing exact handoff or closure semantics,
  or a broader readiness / boundary claim than the accumulated evidence
  supports.

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

- Do not widen from the inherited current architecture into cyclic search,
  multi-SCC search, equi-recursive reasoning, fallback behavior, or a second
  interface unless a later accepted roadmap revision authorizes it explicitly.
- Do not rewrite already-used predecessor roadmap revisions.
- Do not treat one bounded slice as a repo-level readiness claim unless the
  active item explicitly owns that aggregate decision and the evidence is
  sufficient.
- Do not weaken fail-closed ambiguity or soundness obligations to force a
  positive result.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must not continue automatically until a human or an explicit
  recovery recommendation reopens the round
