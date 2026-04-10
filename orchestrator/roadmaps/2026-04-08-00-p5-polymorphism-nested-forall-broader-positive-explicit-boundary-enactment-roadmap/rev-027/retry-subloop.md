# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-027`

## Scope

`rev-027` is a terminal completion publication. All milestones are already
done, so no normal same-round retry loop remains inside this family.

Unexpected or stale controller dispatches under `rev-027` are limited to
recovery of an invalid reopen attempt, including:

- trying to reopen `milestone-4` after merged closeout commit `eaf2256`;
- trying to relitigate merged milestone-3 anchors already carried by
  `7616109` and `ea8db76`;
- overclaiming beyond the accepted nonuple frontier or treating
  `sameLaneAliasFrameClearBoundaryExpr` as whole-family closure;
- reopening `sameLaneDecupleAliasFrameClearBoundaryExpr`, deeper alias
  shells, `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, or
  `N6 termination-pressure` as if `rev-027` authorized that work; or
- trying to use this revision publication to edit
  `orchestrator/state.json`,
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or
  `orchestrator/retry-subloop.md`
  instead of publishing only the successor bundle files.

Review may not return the same round to `plan` inside `rev-027`, because no
lawful extracted item remains live in this completed family.

## Machine State

`orchestrator/state.json` must carry:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `retry: null` when idle, or a recovery note when controller is repairing an
  invalid stale dispatch

## Review Output

Any stale review observed under `rev-027` must record:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed combinations:

- `rejected + update-roadmap`

Forbidden combinations:

- `accepted + finalize`
- `rejected + retry`
- `rejected + finalize`

## Transition Rules

After review of an invalid stale dispatch:

- `rejected + update-roadmap`
  - controller records the invalid reopen attempt
  - controller clears any stale retry bookkeeping that assumed a live
    extracted item still existed
  - controller re-enters `update-roadmap` or terminal completion handling
    instead of returning the same round to `plan`

## Boundary Rules

- Do not reopen the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary inside `rev-027`.
- Do not revise the merged milestone-3 anchor chain or the merged milestone-4
  closeout as if they were still pending work.
- Do not treat `eaf2256` as fresh code/test evidence; its role is the
  docs-only closeout on top of the `ea8db76` code/test baseline.
- Do not reopen
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, or
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as substitute continuation surfaces from inside this completed revision.
- Do not use the `rev-027` publication step to edit controller-owned state or
  pointer stubs. Controller activation after publication may later align those
  surfaces, but that is outside this revision-publication diff.

## Escalation

One stale-dispatch rejection is sufficient to leave the normal retry loop.

After an invalid stale dispatch is recorded under `rev-027`:

- controller must repair or clear the stale dispatch through
  `update-roadmap` or terminal completion handling, not through another
  same-round retry;
- controller must prefer the normal recovery ladder for stale controller
  state before asking a human to reopen anything; and
- same-mechanism retry exhaustion is not applicable, because no lawful live
  extraction remains inside `rev-027`.
