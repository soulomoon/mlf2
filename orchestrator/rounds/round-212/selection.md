# Round 212 - Task Selection

**Selected milestone**: `milestone-3`
**Selected direction**:
`direction-3a-expand-the-broader-positive-representative-corpus`
**Selected extraction**:
`promote-same-lane-clear-boundary-packet-to-first-explicit-milestone-3-representative-corpus-anchor`
**Current task/title**:
`Promote sameLaneClearBoundaryExpr from carry-forward control to the first explicit merged-baseline milestone-3 representative packet on both authoritative entrypoints while preserving the round-211 nested-forall win, predecessor alias-frame truth, checked parity, and fail-closed quantified guards`

**Round metadata**:
- round_id: `round-212`
- branch:
  `orchestrator/round-212-expand-p5-broader-positive-representative-corpus`
- worktree_path: `orchestrator/worktrees/round-212`
- active_round_dir: `orchestrator/rounds/round-212`

**Roadmap identity**:
- roadmap_id:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-015`
- roadmap_dir:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-015`
- milestone_id: `milestone-3`
- direction_id:
  `direction-3a-expand-the-broader-positive-representative-corpus`
- extracted_item_id:
  `promote-same-lane-clear-boundary-packet-to-first-explicit-milestone-3-representative-corpus-anchor`

## Why now

`rev-015` records `milestone-2` as complete on the merged `round-211`
baseline (`5b775b2`) and makes `milestone-3` the lowest-numbered unfinished
milestone in this serial family. With `retry = null`, no same-round retry
state displaces the normal next selection, so the lawful next move is the
first concrete extraction under
`direction-3a-expand-the-broader-positive-representative-corpus`.

The merged `round-211` result already changed the family truth materially:
the selected same-wrapper nested-`forall` packet now stays recursive on both
`runPipelineElab` and `runPipelineElabChecked`, while the preserved focused
matrix, thesis gate, and full repo gate remain green. `milestone-3` therefore
must widen from that one selected packet to the remaining representative
broader-positive corpus rather than reheating milestone-2 freshness, merge
scope, or helper-scaffold debt.

Inside the current representative corpus anchored in
`test/Research/P5ClearBoundarySpec.hs`, the next honest extraction is
`sameLaneClearBoundaryExpr`:

- `nestedForallContrastExpr` is the packet already repaired and merged by
  `round-211`;
- `sameLaneAliasFrameClearBoundaryExpr` is preserved predecessor truth from
  accepted `round-197`, not fresh live debt to reopen; and
- `sameLaneClearBoundaryExpr` is the remaining clear-boundary representative
  packet still carried mostly as control/predecessor framing rather than as an
  explicit `rev-015` milestone-3 corpus-expansion anchor on top of the merged
  round-211 baseline.

That makes the next bounded move concrete: prove that the merged broader
positive mechanism is not just one repaired nested-`forall` packet, but also
an explicit clear-boundary representative packet on both authoritative
entrypoints, without reopening the fail-closed quantified contrasts, the
checked-authoritative parity guards, or the preserved alias-frame predecessor
lane.

## Current baseline

- `orchestrator/state.json` already points `round-212` at
  `milestone-3` /
  `direction-3a-expand-the-broader-positive-representative-corpus`
  under `rev-015`, with branch
  `orchestrator/round-212-expand-p5-broader-positive-representative-corpus`
  and canonical worktree `orchestrator/worktrees/round-212`.
- The controlling merged baseline remains accepted `round-211`, merged as
  `5b775b2`. Preserve its landed writable-slice payload in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  as the starting point rather than reopened milestone-2 draft debt.
- Preserve the green wins explicitly named by `rev-015` and the merged
  `round-211` review:
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints,
  checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  the correct semantic `g g` failure,
  the A6 / nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test`.
- Preserve accepted `round-197` as predecessor truth only:
  `sameLaneAliasFrameClearBoundaryExpr` is one settled retained-child
  clear-boundary lane, not the next live task packet.
- The concrete representative-corpus anchors for this extraction are already
  present:
  `test/Research/P5ClearBoundarySpec.hs` names
  `sameLaneClearBoundaryExpr`,
  `test/PipelineSpec.hs` carries the matching same-lane retained-child exact
  packet authoritative-success rows, and
  `test/ElaborationSpec.hs` carries
  `same-lane retained-child exact edge 3 authoritative instantiation`.
- Keep the preserved negative and fail-closed guard cluster closed:
  do not reopen the old bounded `P2` packet, `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, `N6 termination-pressure`, or any broader
  quantified-crossing contrast as substitute live scope.

## Scope

- Keep this round faithful to exactly one
  `milestone-3` / `direction-3a` extraction:
  `promote-same-lane-clear-boundary-packet-to-first-explicit-milestone-3-representative-corpus-anchor`.
- Keep the selected packet exact:
  `sameLaneClearBoundaryExpr` is the only new representative broader-positive
  packet this round may advance. Do not swap back to the already-merged
  selected same-wrapper nested-`forall` packet, do not reopen the settled
  alias-frame packet as live debt, and do not widen into double-/triple-alias
  descendants or closeout work.
- Treat the matching evidence surface as the bounded planning target:
  `test/Research/P5ClearBoundarySpec.hs` for the exact packet anchor,
  `test/PipelineSpec.hs` for the authoritative-entrypoint representative rows,
  and
  `test/ElaborationSpec.hs` for the same-lane retained-child authoritative
  instantiation guard. If production edits are truly needed, they must stay
  inside the existing `rev-015` writable slice and remain subordinate to this
  exact packet.
- Preserve the merged `round-211` nested-`forall` success, the accepted
  alias-frame predecessor truth, the checked-authoritative parity guard, and
  the current fail-closed quantified contrasts while making the
  `sameLaneClearBoundaryExpr` representative-corpus claim explicit and honest.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  closed read-only continuity anchors unless planning proves a within-slice
  change is strictly required to make this exact packet honest.
- Do not widen into milestone-4 closeout, freshness/merge-debt cleanup,
  fallback rescue, a second interface, cyclic or multi-SCC search,
  equi-recursive reasoning, or any restatement that treats one repaired packet
  plus one predecessor lane as full representative-corpus closure.
