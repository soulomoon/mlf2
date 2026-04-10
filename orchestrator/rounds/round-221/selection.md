# Round 221 - Task Selection

**Selected milestone**: `milestone-4`
**Selected direction**:
`direction-4a-publish-broader-positive-enactment-closeout`
**Selected extraction**:
`publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`
**Current task/title**:
`Publish the docs-only broader-positive enactment closeout that records the merged nonuple frontier on both authoritative entrypoints, preserves sameLaneAliasFrameClearBoundaryExpr as predecessor truth only, keeps the accepted decuple/deeper fail-closed boundary closed, and updates repo-facing notes only if the accepted evidence requires it.`

**Round metadata**:
- round_id: `round-221`
- branch:
  `orchestrator/round-221-publish-broader-positive-enactment-closeout`
- worktree_path: `orchestrator/worktrees/round-221`
- active_round_dir: `orchestrator/rounds/round-221`

**Roadmap identity**:
- roadmap_id:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-026`
- roadmap_dir:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026`
- milestone_id: `milestone-4`
- direction_id:
  `direction-4a-publish-broader-positive-enactment-closeout`
- extracted_item_id:
  `publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`

## Why now

`milestone-4` is now the lowest-numbered unfinished milestone in `rev-026`,
and `retry = null` means no same-round retry state displaces normal forward
selection.

Accepted `round-220`, now merged as base-branch `HEAD = ea8db76`, exhausted
the milestone-3 publication ladder by making
`sameLaneNonupleAliasFrameClearBoundaryExpr` the next explicit
clear-boundary anchor on the research, pipeline, and elaboration surfaces.
`rev-026` states directly that no further milestone-3 extraction remains live
and that the next lawful work is milestone-4 closeout. The active roadmap's
pending extraction is therefore exactly
`publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`.

That choice is concrete and honest on the merged baseline. The enacted
broader-positive frontier is already merged and review-backed on both
authoritative entrypoints, but the repo still lacks the canonical docs-only
closeout artifact that names the exact frontier now earned, records the
preserved closed boundaries honestly, and syncs repo-facing notes only where
the accepted evidence really requires it. Any other immediate move would
either relitigate settled milestone-3 lineage or overstep the merged
`ea8db76` record.

## Current baseline

- `orchestrator/state.json` in the canonical round worktree points
  `round-221` at `milestone-4` /
  `direction-4a-publish-broader-positive-enactment-closeout` under
  `rev-026`, with branch
  `orchestrator/round-221-publish-broader-positive-enactment-closeout`
  and canonical worktree `orchestrator/worktrees/round-221`.
- The controlling merged baseline is base-branch `HEAD = ea8db76`, whose
  title is
  `Promote sameLaneNonupleAliasFrameClearBoundaryExpr to the next milestone-3 anchor`.
  Preserve the merged `round-211` production/test payload plus the merged
  `round-212` through `round-220` evidence-surface promotions already landed
  in `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`.
- Preserve the green merged behavior and gates named by `rev-026` and the
  accepted `round-220` review chain:
  the selected same-wrapper nested-`forall` packet stays recursive on both
  authoritative entrypoints;
  `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth only;
  the explicit broader-positive frontier now runs from
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr`;
  the accepted decuple fail-closed frontier and deeper alias shells remain
  outside live positive support;
  checked-authoritative parity remains green;
  `./scripts/thesis-conformance-gate.sh` passed; and
  `cabal build all && cabal test` passed with `1365 examples, 0 failures`.
- Preserve accepted `round-190` and `round-191` as the controlling evidence
  that the decuple frontier stays fail-closed on both authoritative
  entrypoints. Those rounds remain guard/control truth only; they are not
  leftover milestone-3 debt and may not be reclassified as positive support.
- Treat `rev-022` and `rev-023` as stale unusable recovery publications only.
  They do not participate in the accepted post-round-220 coordination
  lineage, and this selection must stay grounded in `rev-026`.
- The current repo baseline still lacks the milestone-4 closeout record:
  there is no canonical `docs/plans/` artifact yet that names the merged
  nonuple frontier and its preserved closed boundaries on top of `ea8db76`,
  and repo-facing notes such as `TODO.md`,
  `implementation_notes.md`, and
  `CHANGELOG.md`
  do not yet summarize this family closeout.
- Per the accepted family contract,
  `milestone-4` owns repo-facing note updates such as `TODO.md`,
  `implementation_notes.md`, and `CHANGELOG.md` after milestone-3 evidence is
  accepted.
- Treat the canonical round worktree as the authoritative observation surface
  for this selection artifact, and keep its existing controller-owned
  `orchestrator/state.json` / pointer-stub modifications out of scope for
  this selection.

## Scope

- Keep this round docs-only, closeout-only, serial, and faithful to exactly
  one `milestone-4` / `direction-4a` extraction:
  `publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`.
- Publish one canonical closeout artifact under `docs/plans/`, grounded in
  the merged `ea8db76` baseline, that names the exact broader-positive
  frontier now earned on both `runPipelineElab` and
  `runPipelineElabChecked`:
  the merged selected same-wrapper nested-`forall` packet plus the explicit
  clear-boundary anchors from `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr`.
- Record the preserved closed boundaries honestly:
  `sameLaneAliasFrameClearBoundaryExpr` stays predecessor truth only,
  the accepted decuple frontier stays fail-closed,
  deeper alias shells stay outside the live extraction, and preserved `P2`,
  `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
  `N6 termination-pressure` guardrails stay closed rather than reopening as
  substitute scope.
- Update repo-facing notes only if the accepted evidence genuinely requires
  it, keeping any such sync bounded to surfaces such as `TODO.md`,
  `implementation_notes.md`, and `CHANGELOG.md`. Do not invent broader note
  churn, and do not add thesis-facing changes unless the closeout evidence
  actually requires explicit thesis-deviation accounting.
- Keep milestone-3 closed: do not reopen
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr` as live publication debt, do
  not reopen production/test work, and do not convert the merged frontier
  into authorization for decuple/deeper alias support.
- Do not widen into production edits, test edits, Cabal edits, roadmap or
  controller-state edits, cyclic or multi-SCC search, equi-recursive
  reasoning, fallback rescue, a second interface, or any follow-on family
  routing beyond this exact docs-only closeout.
