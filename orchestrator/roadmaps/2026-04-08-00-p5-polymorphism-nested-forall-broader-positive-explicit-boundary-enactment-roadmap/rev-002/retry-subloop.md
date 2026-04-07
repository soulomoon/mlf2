# Retry Subloop Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-002`

## Scope

- `milestone-1` retries for:
  stale or incomplete `round-203` / `round-204` / `round-205` lineage,
  an under-specified broader-positive frontier,
  a missing authoritative-entrypoint success bar,
  a missing representative corpus,
  or an overbroad writable slice.
- `milestone-2` retries for:
  code outside the `rev-002` writable slice,
  unchanged controlling polymorphic-mediation `mu`-absorption behavior in the
  selected slice,
  unsupported semantic widening,
  missing focused tests,
  failing build/test evidence,
  an attempted authoritative annotation-translation repair that still leaves
  the selected packet at the same Phase 6 `PhiTranslatabilityError`,
  or a same-round edit attempt against round-208 artifacts instead of a fresh
  follow-on round.
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
  behavior, or a second interface unless a later accepted roadmap revision
  authorizes it explicitly.
- Do not widen the `rev-002` milestone-2 continuation beyond the exact
  authoritative annotation translation path centered on
  `src/MLF/Elab/Elaborate/Annotation.hs` and the exact companion surfaces
  `src/MLF/Elab/Legacy.hs` and `test/ElaborationSpec.hs` unless a later
  accepted roadmap revision authorizes more.
- Treat `round-208` as immutable blocked predecessor evidence only. Continue
  milestone-2 with a fresh round; do not edit round-208 worktree artifacts.
- Do not reopen `P2` or the representative negative-family rows as substitute
  positive support for the broader-positive frontier.
- Do not treat one repaired packet or one entrypoint-only success as full
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
