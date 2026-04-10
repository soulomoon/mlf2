# Round 214 - Task Selection

**Selected milestone**: `milestone-3`
**Selected direction**:
`direction-3a-expand-the-broader-positive-representative-corpus`
**Selected extraction**:
`promote-same-lane-triple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
**Current task/title**:
`Promote sameLaneTripleAliasFrameClearBoundaryExpr from inherited guard/control evidence to the next explicit merged-baseline milestone-3 representative corpus anchor on both authoritative entrypoints while preserving the merged sameLaneClearBoundaryExpr first anchor, the merged sameLaneDoubleAliasFrameClearBoundaryExpr next anchor, predecessor alias-frame truth, checked parity, and the no-quadruple-alias boundary`

**Round metadata**:
- round_id: `round-214`
- branch:
  `orchestrator/round-214-promote-p5-triple-alias-clear-boundary-anchor`
- worktree_path: `orchestrator/worktrees/round-214`
- active_round_dir: `orchestrator/rounds/round-214`

**Roadmap identity**:
- roadmap_id:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-017`
- roadmap_dir:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-017`
- milestone_id: `milestone-3`
- direction_id:
  `direction-3a-expand-the-broader-positive-representative-corpus`
- extracted_item_id:
  `promote-same-lane-triple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`

## Why now

`rev-017` records `milestone-3` as the lowest-numbered unfinished milestone on
top of merged base-branch `HEAD = 2091c39`, and `retry = null` means no
same-round retry state displaces the normal next selection.

Accepted `round-213`, now merged as `2091c39`, exhausted the second
milestone-3 publication step by making
`sameLaneDoubleAliasFrameClearBoundaryExpr` the next explicit clear-boundary
anchor on the research, pipeline, and elaboration surfaces. `rev-017` names
the lawful next unfinished extraction directly:
`promote-same-lane-triple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`.

That choice is concrete and honest on the merged baseline. The current tree
already preserves `sameLaneTripleAliasFrameClearBoundaryExpr` as green
inherited guard/control evidence in `test/PipelineSpec.hs`, but it has not
yet been promoted to the next explicit milestone-3 representative anchor in
`test/Research/P5ClearBoundarySpec.hs` plus the matching authoritative
pipeline/elaboration guards. The merged `sameLaneClearBoundaryExpr` first
anchor, the merged `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor,
the preserved `sameLaneAliasFrameClearBoundaryExpr` predecessor lane, and the
merged selected same-wrapper nested-`forall` packet therefore remain baseline
truth only, not whole-frontier closure.

## Current baseline

- `orchestrator/state.json` and the canonical round-worktree state both point
  `round-214` at `milestone-3` /
  `direction-3a-expand-the-broader-positive-representative-corpus` under
  `rev-017`, with branch
  `orchestrator/round-214-promote-p5-triple-alias-clear-boundary-anchor`
  and canonical worktree `orchestrator/worktrees/round-214`.
- The controlling merged baseline is base-branch `HEAD = 2091c39`. Preserve
  the merged `round-211` production/test payload in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the merged `round-212` and merged `round-213` evidence-surface
  promotions already landed in the three test files.
- Preserve the green merged wins named by `rev-017`:
  `sameLaneClearBoundaryExpr` as the first explicit milestone-3
  clear-boundary anchor,
  `sameLaneDoubleAliasFrameClearBoundaryExpr` as the next explicit
  milestone-3 clear-boundary anchor,
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints,
  `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only,
  checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  the correct semantic `g g` failure,
  the A6 / nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1344 examples, 0 failures`.
- On this baseline, `test/PipelineSpec.hs` already contains bounded
  triple-alias and quadruple-alias clear-boundary continuity probes, but
  `test/Research/P5ClearBoundarySpec.hs` does not yet publish
  `sameLaneTripleAliasFrameClearBoundaryExpr` as an explicit milestone-3
  representative anchor, and `test/ElaborationSpec.hs` does not yet carry the
  matching exact-edge authoritative-instantiation guard for that packet.
- Keep the negative/fail-closed quantified contrasts honest:
  do not reopen `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`,
  `N6 termination-pressure`, or any broader quantified-crossing contrast as
  substitute live scope.
- Keep the canonical round-worktree's existing controller-owned
  `orchestrator/state.json` / pointer-stub modifications out of scope for this
  selection.

## Scope

- Keep this round faithful to exactly one `milestone-3` / `direction-3a`
  extraction:
  `promote-same-lane-triple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`.
- Keep the selected packet exact:
  `sameLaneTripleAliasFrameClearBoundaryExpr` is the only new representative
  broader-positive packet this round may advance. Do not reopen the already
  merged `sameLaneClearBoundaryExpr` first anchor or the already merged
  `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor as live debt, do not
  reopen `sameLaneAliasFrameClearBoundaryExpr` beyond preserved predecessor
  truth, and do not widen into quadruple/deeper alias shells or milestone-4
  closeout.
- Treat the bounded evidence surface as:
  `test/Research/P5ClearBoundarySpec.hs` for the next explicit milestone-3
  anchor,
  `test/PipelineSpec.hs` for the matching authoritative-entrypoint rows, and
  `test/ElaborationSpec.hs` for the exact-edge authoritative-instantiation
  guard. If production edits are truly needed, they must stay inside the
  existing `rev-017` writable slice and remain subordinate to this exact
  packet.
- Preserve the merged `sameLaneClearBoundaryExpr` first anchor, the merged
  `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, the merged selected
  same-wrapper nested-`forall` packet, the accepted alias-frame predecessor
  truth, checked-authoritative parity, and the current fail-closed quantified
  contrasts while making the triple-alias representative-corpus claim explicit
  and honest.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  closed read-only continuity anchors unless a later accepted roadmap revision
  authorizes a widening explicitly.
- Do not widen into fallback rescue, a second interface, cyclic or multi-SCC
  search, equi-recursive reasoning, quadruple-/deeper-alias substitute
  closure, or any restatement that treats the merged first two anchors plus
  predecessor lanes as full broader-positive frontier closure.
