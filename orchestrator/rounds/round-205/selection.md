# Round 205 - Task Selection

**Selected milestone**: `milestone-3`
**Selected direction**:
`direction-3a-bind-one-downstream-consequence`
**Selected extraction**:
`bind-one-downstream-consequence`
**Current task/title**:
`Bind one exact downstream consequence from the revised planning ledger`

**Roadmap identity**:
- roadmap_id: `2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`
- roadmap_revision: `rev-002`
- roadmap_dir: `orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-002`
- milestone_id: `milestone-3`
- direction_id: `direction-3a-bind-one-downstream-consequence`
- extracted_item_id: `bind-one-downstream-consequence`

## Why now

Accepted `round-204` completed `milestone-2` under `rev-002` and fixed the
exact broader positive `P5 polymorphism-nested-forall` frontier plus the
preserved-versus-live-pressure split that this family now has to consume.
The active roadmap revision marks `milestone-1` and `milestone-2` done, and
its `milestone-2` completion notes explicitly route the family directly to
`milestone-3` without reopening a separate `direction-2b` comparison packet.

That makes `milestone-3` /
`direction-3a-bind-one-downstream-consequence`
the lowest-numbered lawful unfinished item. The family cannot end with only a
frozen broader ledger; it now has to bind exactly one downstream consequence
from the accepted `round-203` / `round-204` planning record and explain why
all non-selected routes stay closed.

## Current baseline

- Round-205 branch/worktree `HEAD`: `465e68f`
- Controller-prepared round branch remains:
  `orchestrator/round-205-bind-one-downstream-consequence`
- Controller-prepared round worktree remains: `orchestrator/worktrees/round-205`
- Active roadmap family remains
  `2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`
  at `rev-002`
- `milestone-1` is complete through accepted `round-203`
- `milestone-2` is complete through accepted `round-204`
- `milestone-3` is the only remaining pending milestone
- No retry state is active (`retry: null`, no `resume_error`)
- Accepted `round-203` published the controlling milestone-1 refreeze artifact
  at
  `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md`
  and recorded approval in
  `orchestrator/rounds/round-203/review-record.json`
- Accepted `round-204` published the controlling milestone-2 broader-ledger
  artifact at
  `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md`
  and recorded approval in
  `orchestrator/rounds/round-204/review-record.json`
- That accepted broader ledger fixes the exact broader frontier to positive
  nested-forall / quantified-crossing support beyond the one settled
  retained-child lane, preserves the revised milestone-1 refreeze as
  controlling, and routes the family directly to one final downstream bind
  rather than a separate `direction-2b` reread
- Accepted `round-197` still keeps
  `sameLaneAliasFrameClearBoundaryExpr`
  as one bounded retained-child clear-boundary success on
  `runPipelineElab` / `runPipelineElabChecked`, while
  `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`
- Accepted `round-191` / `round-181` still keep `P2` packet-bounded to the
  exact `C1` packet only, and accepted `round-192` still keeps
  `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and
  `N6 termination-pressure`
  closed as predecessor truth only
- Repository status in the canonical round worktree before writing this
  selection was controller-owned `orchestrator/state.json` only

## Scope

- Keep this round docs-only, planning-only, serial, and faithful to exactly
  one milestone-3 / direction-3a extraction:
  bind one exact downstream consequence from the revised planning ledger
- Publish one canonical final handoff artifact under `docs/plans/` plus round
  bookkeeping only; that artifact must consume the accepted `round-203` /
  `round-204` planning record, name exactly one downstream consequence, and
  explain why every non-selected route remains closed
- Ground the handoff in the accepted revised-freeze lineage, especially:
  `round-204`, `round-203`, `round-201`, `round-200`, `round-197`,
  `round-192`, `round-191`, `round-181`,
  `implementation_notes.md`, and
  `test/Research/P5ClearBoundarySpec.hs`
- Keep the family planning-only: do not revise code, tests, Cabal wiring, or
  the inherited boundary itself, and do not enact the selected downstream
  route inside `rev-002`
- Do not reopen `P2`, the March 28 exact packet, the representative
  negative-family rows, or the one settled retained-child lane as live debt
- Do not resurrect a separate `direction-2b` comparison packet, do not bind
  more than one downstream consequence, and do not treat the family as closed
  merely because one retained-child lane already succeeds
