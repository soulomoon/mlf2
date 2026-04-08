# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-006`

## Scope

- `milestone-1` retries for:
  stale or incomplete `round-203` / `round-204` / `round-205` lineage,
  an under-specified broader-positive frontier,
  a missing authoritative-entrypoint success bar,
  a missing representative corpus,
  or an overbroad writable slice.
- `milestone-2` retries for:
  code outside the preserved `round-211` live baseline plus the one newly
  admitted `rev-006` seam,
  unchanged controlling polymorphic-mediation `mu`-absorption behavior in the
  selected slice,
  unsupported semantic widening,
  missing focused tests,
  failing build/test evidence,
  an attempted same-round `Algebra.hs` repair that regresses the selected
  packet back to the old Phase 6 `reifyInst` stop or the downstream
  `PhiReorder: missing binder identity` detour,
  an attempted same-round repair that still leaves the selected packet
  blocked either at the original Phase 7 `TCArgumentMismatch` or at the
  immediate Phase 7 `TCLetTypeMismatch` let-scheme / closure contradiction
  already proved by the `rev-005` `AAppF`-only attempt,
  or any attempt that discards the preserved `round-211` diff, restarts on a
  fresh round, or reopens pipeline/handoff/public/fallback seams instead of
  clearing the exact `ALetF` continuation.
- `milestone-3` retries for:
  broader-positive claims not backed by success on both `runPipelineElab` and
  `runPipelineElabChecked`,
  missing representative corpus coverage,
  public/internal parity drift,
  or reopened `P2` / `N1` / `N2` / `N6` obligations.
- `milestone-4` retries for:
  closeout claims broader than the accepted code/test evidence,
  missing repo-facing notes,
  missing thesis-deviation accounting when required,
  or a recorded controlling-behavior change that the enacted family did not
  actually earn.

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

- Do not widen from the accepted explicit-only / iso-recursive boundary into
  cyclic search, multi-SCC search, equi-recursive reasoning, fallback
  rescue, or a second interface unless a later accepted roadmap revision
  authorizes it explicitly.
- Do not widen the `rev-006` milestone-2 continuation beyond the preserved
  `round-211` baseline in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the one newly admitted same-file seam
  `src/MLF/Elab/Elaborate/Algebra.hs`
  unless a later accepted roadmap revision authorizes more.
- Treat `round-208`, `round-209`, and `round-210` as immutable blocked
  predecessor evidence only. Continue milestone-2 on the same `round-211`
  branch/worktree baseline; do not edit any blocked round worktree artifacts,
  and do not discard the current `round-211` diff on a fresh round.
- Do not reopen
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, or
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as substitute continuation surfaces unless a later accepted roadmap
  revision authorizes them explicitly.
- Do not broaden `src/MLF/Elab/Elaborate/Algebra.hs` beyond the selected
  `ALetF` let-scheme finalization / closure logic around scheme selection,
  `closeTermWithSchemeSubstIfNeeded`, and
  `rhsFinal`, except for the exact surrounding `AAppF` context needed
  inside that same file to keep the selected packet coherent.
- Do not reopen `P2` or the representative negative-family rows as substitute
  positive support for the broader-positive frontier.
- Do not treat one repaired packet or one boundary-crossing seam repair as
  full broader-positive frontier completion unless the active milestone
  explicitly owns only that narrower claim.
- Do not regress the accepted retained-child clear-boundary lane while
  enacting the broader-positive frontier.
- Do not rewrite already-used predecessor roadmap families or revisions.

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
