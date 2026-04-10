# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-010`

## Scope

- `milestone-1` retries for:
  stale or incomplete `round-203` / `round-204` / `round-205` lineage,
  an under-specified broader-positive frontier,
  a missing authoritative-entrypoint success bar,
  a missing representative corpus,
  or an overbroad writable slice.
- `milestone-2` retries for:
  code outside the preserved live `round-211` baseline plus the newly admitted
  `rev-010` seam,
  loss of the current `round-211` gains on the selected packet,
  checked-authoritative parity, or classic let-polymorphism /
  explicit-`forall` positives,
  unchanged controlling behavior in the newly admitted post-closure
  authoritative classification / witness refinement seam,
  missing focused evidence for the remaining A6 / nested-let fail-fast
  cluster,
  failing build/test evidence or failing thesis-conformance evidence,
  an attempted repair that regresses the selected packet back to the old Phase
  6 stop, the downstream `PhiReorder: missing binder identity` detour, the old
  Phase 7 `TCArgumentMismatch`, or the immediate `ALetF`
  `TCLetTypeMismatch` chain already cleared by the inherited Algebra repair,
  an attempted repair that still leaves `BUG-2026-02-06-002` or the A6
  dual-coercion pair at `TCExpectedArrow` after the admitted post-closure seam
  has been exercised,
  an attempted repair that still leaves the nested-let fail-fast / invariant
  probes at alias-side `TCLetTypeMismatch` after the admitted seam has been
  exercised,
  an attempted repair that removes the alias-side mismatch only by turning the
  nested-let fail-fast probe into false success `forall a. a -> a`,
  or any attempt that discards the preserved `round-211` diff, restarts on a
  fresh round, retries `TermClosure.hs` as if `rev-009` were still open, or
  reopens pipeline/public/fallback surfaces instead of clearing the exact
  selected post-closure authoritative classification / witness seam.
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
  or a recorded controlling-behavior change the enacted family did not earn.

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
  cyclic search, multi-SCC search, equi-recursive reasoning, fallback rescue,
  or a second interface unless a later accepted roadmap revision authorizes it
  explicitly.
- Do not widen the `rev-010` milestone-2 continuation beyond the preserved
  `round-211` baseline in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the newly admitted post-closure authoritative classification / witness
  seam.
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
- Do not relitigate the inherited selected-packet `Algebra.hs` repair as if
  the packet were still unresolved. Preserve that repair and only adjust the
  exact post-closure authoritative classification / handoff locals
  `rhsAbs0`, `rhsAbs`, `rhsFinal`, and `bodyElab` / `env'`.
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
- Do not reopen `P2` or representative negative-family rows as substitute
  positive support for the broader-positive frontier.
- Do not treat one repaired packet or one repaired fail-fast cluster as full
  broader-positive frontier completion unless the active milestone explicitly
  owns only that narrower claim.
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
