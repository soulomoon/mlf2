# Round 183 — Task Selection

**Selected item**: item-5
**Item title**: Run the bounded positive-family implementation and evidence campaign
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-5

## Why now

`item-5` is still the lowest-numbered unfinished roadmap item, its
dependencies `item-1` through `item-4` are done, and no live retry state
forces a same-round retry.

Accepted `round-181` settled the first bounded item-5 slice on the exact `C1`
non-local scheme-alias / base-like packet by removing the packet-local
`Run/Pipeline` shortcut while keeping recursive authoritative output on both
entrypoints. Accepted `round-182` then made the same-lane baseline more
honest by narrowing the shared retained-child preservation seam in
`src/MLF/Elab/TermClosure.hs` so
`sameLaneAliasFrameClearBoundaryExpr` remains recursive only through the exact
clear-boundary rule that the current code still earns.

The next honest move is therefore to stay on item-5 and challenge that newly
narrowed shared seam against the immediately adjacent same-lane follow-on
packet already frozen by predecessor authority:
`sameLaneDoubleAliasFrameClearBoundaryExpr`. That packet is still accepted
predecessor truth only, and after accepted `round-182` it has only been rerun
as an adjacent control on the tightened baseline, not yet re-earned as a
current item-5 slice.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `307fe94`
  (`Advance full-inference roadmap after round-182`)
- Active roadmap revision: `rev-001`, with items `1` through `4` done and
  items `5` through `7` pending
- Live retry state: none recorded for this family (`retry: null`)
- Accepted family docs from rounds `177` through `180` still control the
  round: predecessor authority freeze, mechanism map, fail-closed
  candidate/ambiguity/termination contract, and reconstruction-visible
  readiness contract are all in place
- Accepted `round-181` settles the exact `C1` packet only: the packet-local
  `Run/Pipeline` shortcut is gone, while `runPipelineElab` and
  `runPipelineElabChecked` still preserve recursive authoritative output for
  that one packet
- Accepted `round-182` settles one current-roadmap same-lane packet only:
  `sameLaneAliasFrameClearBoundaryExpr` remains honest only via the narrowed
  shared clear-boundary retained-child rule in
  `src/MLF/Elab/TermClosure.hs`, with the item-3 retained-child route / guard
  cluster and pipeline facades unchanged
- `sameLaneDoubleAliasFrameClearBoundaryExpr` remains bounded predecessor
  `narrow success` truth from the April 2 follow-on family, and on the
  current baseline it has only been rerun as an adjacent control after the
  `round-182` narrowing
- The admitted retained-child route stays fixed to
  `sameLaneLocalRetainedChildTarget` plus the
  `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard cluster; no new
  owner, binder, search, fallback, or interface family is admitted
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-183-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-183`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round

## Scope

- Keep item-5 active, but bind this round to exactly one bounded same-lane
  retained-child positive-family slice:
  `sameLaneDoubleAliasFrameClearBoundaryExpr` only
- Treat the earlier accepted double-alias `narrow success` as predecessor
  context only; this round must determine whether that exact packet still
  preserves recursive authoritative output on the current full-inference
  baseline after `round-182` narrowed the shared
  `preserveRetainedChildAuthoritativeResult` rule
- Center any implementation work on the smallest current seam implicated by
  that question only: `src/MLF/Elab/TermClosure.hs`, plus the matching exact
  packet assertions in `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  and `test/PipelineSpec.hs` if the bounded packet needs synchronization
- Reuse focused evidence only from the exact retained-child anchors already in
  place: the double-alias coverage in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`; keep `sameLaneAliasFrameClearBoundaryExpr`, the
  clear-boundary `P5` control / nested-`forall` contrast, and the `C1`
  non-local packet as predecessor evidence or adjacent controls only
- Permit only the minimum source/test changes justified by that exact packet
  and the shared retained-child preservation seam; do not reopen general
  same-lane family settlement, do not alter the item-3 fail-closed
  route/ambiguity/termination contract, and do not widen the pipeline or
  fallback surface in place
- End with one honest read of this exact double-alias packet against the
  item-4 reconstruction-visible contract only; do not translate one packet
  into general `P3`, `P4`, or `P6` closure, and do not claim repo-level
  readiness
- Keep the round bounded and non-widening: no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, and no roadmap/controller-state edits
