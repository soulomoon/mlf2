# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-012`

## Scope

- `milestone-2` retries for:
  code outside the preserved live `round-211` baseline plus the newly admitted
  `rev-012` seam,
  loss of the current protected wins on the selected packet,
  unchanged controlling behavior in the broader authoritative application /
  let-polymorphism handoff,
  missing focused evidence for the remaining A6 dual-consumer,
  nested-let fail-fast, or representative let-polymorphism /
  representative-corpus regressions,
  failing build/test evidence or failing thesis-conformance evidence,
  an attempted repair that regresses the selected packet, checked-authoritative
  parity, `BUG-2026-02-06-002`, retained-child packet, `BUG-2026-02-17-002`,
  or `g g`,
  or any attempt that retries `TermClosure.hs`, pipeline/public/fallback
  surfaces, cyclic widening, equi-recursive reasoning, or a second interface
  instead of clearing the exact wider handoff named by `rev-012`.

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
  - controller enters `update-roadmap` on the same live round baseline

## Boundary Rules

- Do not widen from the accepted explicit-only / iso-recursive boundary into
  cyclic search, multi-SCC search, equi-recursive reasoning, fallback rescue,
  or a second interface unless a later accepted roadmap revision authorizes it
  explicitly.
- Do not widen the `rev-012` milestone-2 continuation beyond the preserved
  `round-211` baseline in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the newly admitted broader authoritative application /
  let-polymorphism handoff in `Algebra.hs` and its immediate authoritative
  companion.
- Treat `round-208`, `round-209`, and `round-210` as immutable blocked
  predecessor evidence only. Continue milestone-2 on the same `round-211`
  branch/worktree baseline; do not edit blocked round artifacts, and do not
  discard the current `round-211` diff on a fresh round.
- Do not reopen
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, or
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as substitute continuation surfaces unless a later accepted roadmap revision
  authorizes them explicitly.
- Do not relitigate the inherited selected-packet or `BUG-2026-02-17-002`
  repairs as if they were still unresolved. Preserve those as settled baseline
  and only adjust the newly admitted broader handoff.
- Do not broaden `src/MLF/Elab/Elaborate/Annotation.hs` beyond the
  `reifyInst` authoritative refinement helpers
  `authoritativeTargetType`,
  `inferAuthoritativeInstArgs`,
  `reifyTraceBinderInstArgs`,
  `instNeedsAuthoritativeRefinement`, and
  `instSeqApps`.
- Do not broaden `src/MLF/Elab/Legacy.hs` beyond
  `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes`, and only as a
  direct mechanical companion to the admitted `Annotation.hs` witness
  refinement.
- Do not treat one repaired packet or one repaired fail-fast cluster as full
  broader-positive frontier completion unless the active milestone explicitly
  owns only that narrower claim.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must automatically enter the same-round recovery ladder before
  asking a human to reopen anything
- same-mechanism retry exhaustion is escalation, not terminal stop
