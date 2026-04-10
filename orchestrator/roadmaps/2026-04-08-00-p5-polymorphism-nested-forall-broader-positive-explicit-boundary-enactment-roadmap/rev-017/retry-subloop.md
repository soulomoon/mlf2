# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-017`

## Scope

- `milestone-3` retries for:
  broader-positive claims not backed by success on both
  `runPipelineElab` and `runPipelineElabChecked`,
  missing representative-corpus coverage beyond the merged selected
  same-wrapper nested-`forall` packet, the merged
  `sameLaneClearBoundaryExpr` first anchor, and the merged
  `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor,
  failure to make `sameLaneTripleAliasFrameClearBoundaryExpr` honest as the
  next explicit milestone-3 representative anchor on the `P5` research /
  pipeline / elaboration surfaces,
  regressions to the merged `2091c39` baseline wins,
  loss of checked-authoritative parity,
  reopening the retained-child predecessor lane as live debt,
  widening from the selected triple-alias packet into quadruple/deeper alias
  shells as substitute closure,
  reopened `P2` / `N1 ambiguity-reject` / `N2 unsoundness-guard` /
  `N6 termination-pressure` obligations,
  failing thesis-conformance or full build/test evidence,
  edits outside the preserved writable slice, or
  any attempt that reopens closed continuity anchors instead of carrying the
  merged milestone-2 baseline and first two merged milestone-3 anchors
  honestly.
- `milestone-4` retries for:
  closeout claims broader than the accepted code/test evidence,
  missing repo-facing notes when the evidence requires them,
  missing thesis-deviation accounting when required, or
  a recorded controlling-behavior change the enacted family did not earn.

Review may reject and return the same round to `plan` only when the latest
attempt lands materially new within-slice evidence or materially narrows a
newly exposed blocker while keeping `rev-017` fit for a direct follow-up.
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
  cyclic search, multi-SCC search, equi-recursive reasoning, fallback rescue,
  or a second interface unless a later accepted roadmap revision authorizes it
  explicitly.
- Do not widen the `rev-017` continuation beyond the preserved writable slice
  in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  unless a later accepted revision authorizes more.
- Treat accepted `round-211`, merged as `5b775b2`, accepted `round-212`,
  merged as `9bb2229`, and accepted `round-213`, merged as `2091c39`, as the
  merged predecessor baseline chain. Do not relitigate the milestone-2
  handoff repair or the first two clear-boundary anchor publications as if
  they were still live blockers.
- Keep the live pointer stubs in both
  `orchestrator/roadmap.md`,
  `orchestrator/retry-subloop.md`, and
  `orchestrator/verification.md`
  surfaces aligned with the active `rev-017` bundle, and do the same for the
  canonical round-worktree pointer stubs when a new revision is published.
- Do not reopen
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, or
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as substitute continuation surfaces unless a later accepted roadmap revision
  authorizes them explicitly.
- Do not treat the merged selected same-wrapper nested-`forall` packet, the
  merged `sameLaneClearBoundaryExpr` first anchor, the merged
  `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, and the preserved
  `sameLaneAliasFrameClearBoundaryExpr` predecessor lane as full
  broader-positive frontier closure.
- Do not reopen the retained-child clear-boundary lane or the current
  fail-closed quantified contrasts as substitute positive support unless new
  accepted evidence really changes their status.
- Do not widen the exact next corpus-promotion packet beyond
  `sameLaneTripleAliasFrameClearBoundaryExpr` unless a later accepted roadmap
  revision authorizes a deeper alias-budget extraction explicitly.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must automatically enter the same-round recovery ladder before
  asking a human to reopen anything
- same-mechanism retry exhaustion is escalation, not terminal stop
