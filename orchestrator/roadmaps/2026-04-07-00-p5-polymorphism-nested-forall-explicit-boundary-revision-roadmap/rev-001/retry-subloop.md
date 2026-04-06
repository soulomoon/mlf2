# Retry Subloop Contract

Roadmap family: `2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`
Revision: `rev-001`

## Scope

- `milestone-1` retries for:
  stale or incomplete inherited-boundary freeze,
  missing or ambiguous decision-surface vocabulary,
  predecessor evidence reopened as live debt without authority,
  or any scope creep outside the planning-only docs surface.
- `milestone-2` retries for:
  a vague broader positive `P5` frontier,
  blurred classification between predecessor truth and live pressure,
  an unsupported current-boundary-versus-explicit-revision comparison,
  or any attempt to reopen `P2` or the representative negative-family rows
  without explicit accepted authority.
- `milestone-3` retries for:
  mixed downstream consequences,
  missing exact handoff semantics,
  unexplained non-selected routes,
  or any attempt to enact code, tests, or a concrete boundary revision instead
  of binding the planning result.

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

- `rev-001` is planning-only. Do not change `src/`, `src-public/`, `app/`,
  `test/`, or `mlf2.cabal`.
- Do not revise the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback boundary inside this
  revision; this family may only plan against that boundary and pressure it
  honestly.
- Do not reopen the accepted retained-child clear-boundary settlement, the
  accepted March 28 exact packet, the accepted `round-151` reclassification,
  or the representative negative-family rows as live debt without fresh
  accepted authority inside this family.
- Do not reopen `P2` or promote the exact `C1` packet into broader family
  closure unless an accepted artifact in this family explicitly changes that
  scope.
- Do not pre-authorize implementation, new representative tests, or concrete
  boundary-revision enactment.
- Do not weaken fail-closed ambiguity or soundness obligations to force a
  broader positive read.

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
