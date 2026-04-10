# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-026`

## Scope

- `milestone-4` retries for:
  closeout claims broader than the accepted `ea8db76` code/test evidence,
  failure to record the exact broader-positive frontier now earned on both
  authoritative entrypoints,
  failure to keep `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth
  only,
  wrongful promotion of the accepted decuple fail-closed frontier or deeper
  alias shells into positive support,
  reopened `P2` / `N1 ambiguity-reject` / `N2 unsoundness-guard` /
  `N6 termination-pressure` obligations,
  missing repo-facing notes when the evidence requires them,
  missing thesis-deviation accounting when required, or
  a recorded controlling-behavior change the enacted family did not earn.

Review may reject and return the same round to `plan` only when the latest
attempt lands materially new within-slice evidence or materially narrows a
newly exposed blocker while keeping `rev-026` fit for a direct follow-up.
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
- `rejected + update-roadmap`

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
- `rejected + update-roadmap`
  - controller records the attempt
  - controller increments `retry.attempt`
  - controller enters `update-roadmap`

## Boundary Rules

- Do not widen from the accepted explicit-only / iso-recursive boundary into
  cyclic search, multi-SCC search, equi-recursive reasoning, fallback
  rescue, or a second interface unless a later accepted roadmap revision
  authorizes it explicitly.
- Treat accepted `round-211`, merged as `5b775b2`, accepted `round-212`,
  merged as `9bb2229`, accepted `round-213`, merged as `2091c39`, accepted
  `round-214`, merged as `ed66291`, accepted `round-215`, merged as
  `1b62ad5`, accepted `round-216`, merged as `21fddba`, accepted
  `round-217`, merged as `f405079`, accepted `round-218`, merged as
  `7a127e2`, accepted `round-219`, merged as `7616109`, and accepted
  `round-220`, merged as `ea8db76`, as the merged predecessor baseline chain.
  Do not relitigate the milestone-2 handoff repair or the milestone-3 anchor
  publications as if they were still live blockers.
- Treat `rev-022` and `rev-023` as stale unusable recovery publications.
  Do not modify them, activate them, or cite them as authoritative lineage.
- Keep the live pointer stubs in
  `orchestrator/roadmap.md`,
  `orchestrator/retry-subloop.md`, and
  `orchestrator/verification.md`
  surfaces aligned with the published `rev-026` bundle, and do the same for
  the canonical round-worktree pointer stubs while the controller-global
  `update-roadmap` handoff is pending.
- Do not reopen
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, or
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as substitute continuation surfaces unless a later accepted roadmap
  revision authorizes them explicitly.
- Do not treat the merged selected same-wrapper nested-`forall` packet, the
  merged explicit clear-boundary anchors from
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr`, or the preserved
  `sameLaneAliasFrameClearBoundaryExpr` predecessor lane as authorization to
  reopen decuple/deeper alias shells as successful support.
- Do not reopen the retained-child clear-boundary lane, the accepted decuple
  fail-closed frontier, or the current fail-closed quantified contrasts as
  substitute positive support unless new accepted evidence really changes
  their status.
- Keep `rev-026` closeout work docs-only unless a later accepted roadmap
  revision explicitly authorizes code/test widening.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must automatically enter the same-round recovery ladder before
  asking a human to reopen anything
- same-mechanism retry exhaustion is escalation, not terminal stop
