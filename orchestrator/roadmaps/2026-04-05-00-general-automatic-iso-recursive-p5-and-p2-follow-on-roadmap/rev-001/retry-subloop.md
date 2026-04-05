# Retry Subloop Contract

Roadmap family: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
Revision: `rev-001`

## Scope

- `milestone-1` retries for:
  stale or incomplete post-item-7 `P5` authority,
  failure to distinguish the settled March 28 exact packet from the still-live
  positive-family blocker,
  a missing exact successor lane or writable slice,
  or a mixed current-architecture / boundary-pressure gate.
- `milestone-2` retries for:
  scope creep outside the milestone-1-selected `P5` lane,
  unsupported semantic widening,
  missing authoritative-surface evidence for the selected lane,
  or a mislabeled fail-closed / positive / boundary-pressure read.
- `milestone-3` retries for:
  ambiguous post-`P5` routing,
  premature promotion of the accepted `P2` packet into family closure,
  a missing exact follow-on lane,
  or a blurred `P5` / `P2` / boundary-pressure classification.
- `milestone-4` retries for:
  mixed or ambiguous end-state outcomes,
  missing exact handoff / enablement semantics,
  or a broader readiness / boundary claim than the refreshed ledger supports.

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
- Do not reopen the settled March 28 exact `P5` packet as live debt inside
  this family.
- Do not promote the accepted exact `C1` / `P2` packet into family closure
  without fresh representative evidence.
- Do not treat the accepted round-151 reclassification of nested-forall
  absorption as if it already settles positive `P5` family support.
- Do not weaken fail-closed ambiguity or soundness obligations to force a
  positive result.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must automatically enter the same-round recovery ladder before
  asking a human to reopen anything
- same-mechanism retry exhaustion is escalation, not terminal stop: re-observe
  the recorded canonical round worktree, apply lawful controller-owned
  recovery if already-produced outputs exist, and then re-dispatch through a
  different lawful recovery path when available
- ask a human to reopen the round only after no lawful same-round recovery
  action remains
