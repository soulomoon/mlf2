# Round 189 - Task Selection

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

Accepted `round-181` through `round-188` settled eight bounded `item-5`
packets only: one exact `P2` non-local packet plus seven exact same-lane
retained-child packets. The active roadmap was updated after `round-188` to
keep `item-5` pending while making the next unfinished work concrete as
exactly one more adjacent same-lane retained-child packet beyond the accepted
septuple case, with fresh octuple probes still failing closed before any
further widening.

The next honest move is therefore to stay on `item-5` and challenge the
smallest fresh same-lane retained-child packet immediately beyond the current
accepted boundary:
`sameLaneOctupleAliasFrameClearBoundaryExpr`, defined as the current
septuple-alias clear-boundary packet plus one more same-lane local alias
binder between `tip` and the final retained-child consumer. No accepted
artifact yet names or settles that exact packet, so selecting it now
preserves the alias, double-alias, triple-alias, quadruple-alias,
quintuple-alias, sextuple-alias, and septuple-alias packets as bounded
predecessor truth only while asking the next concrete question the current
repo baseline actually leaves open.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `2ebf4bf`
  (`Advance full-inference roadmap after round-188`)
- Active roadmap revision: `rev-001`, with items `1` through `4` done and
  items `5` through `7` pending
- Live retry state: none recorded for this family (`retry: null`)
- Accepted family docs from rounds `177` through `180` still control the
  round: predecessor authority freeze, mechanism map, fail-closed
  candidate/ambiguity/termination contract, and reconstruction-visible
  readiness contract are all in place
- Accepted `round-181` settles the exact `C1` non-local
  scheme-alias/base-like packet only: the packet-local `Run/Pipeline`
  shortcut is gone, while `runPipelineElab` and `runPipelineElabChecked`
  still preserve recursive authoritative output for that one packet
- Accepted `round-182` settles one same-lane retained-child packet only:
  `sameLaneAliasFrameClearBoundaryExpr` remains honest only via the narrowed
  shared clear-boundary retained-child rule in
  `src/MLF/Elab/TermClosure.hs`, with the item-3 retained-child route / guard
  cluster and pipeline facades unchanged
- Accepted `round-183` settles one adjacent same-lane retained-child packet
  only: `sameLaneDoubleAliasFrameClearBoundaryExpr` remains honest on the
  authoritative entrypoints only via the existing bounded one-extra-alias-shell
  `TermClosure` rule, and that round needed only exact
  authoritative-output / mechanism-guard test tightening rather than a new
  production edit
- Accepted `round-184` settles one further adjacent same-lane packet only:
  `sameLaneTripleAliasFrameClearBoundaryExpr` remains honest on
  `runPipelineElab` and `runPipelineElabChecked` only via an exact depth-2 /
  two-extra-alias-shell `TermClosure` rule, with the retained-child route /
  guard cluster and pipeline facades unchanged
- Accepted `round-185` settles one further adjacent same-lane packet only:
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr` remains honest on the
  authoritative entrypoints only via extending the shared
  `src/MLF/Elab/TermClosure.hs` retained-child alias-boundary entry budget
  from `1` to `2`, while keeping `hasRetainedChildClearBoundary` as the
  terminal bounded rule and leaving the retained-child route / guard cluster,
  pipeline facades, and fallback seams unchanged
- Accepted `round-186` settles one further adjacent same-lane packet only:
  `sameLaneQuintupleAliasFrameClearBoundaryExpr` remains honest on
  `runPipelineElab` and `runPipelineElabChecked` only via keeping the outer
  `src/MLF/Elab/TermClosure.hs` retained-child alias-boundary entry budget
  fixed at `2` while adding a bounded one-step alias budget inside the
  terminal clear-boundary helper, with the retained-child route / guard
  cluster, pipeline facades, and fallback seams unchanged
- Accepted `round-187` settles one further adjacent same-lane packet only:
  `sameLaneSextupleAliasFrameClearBoundaryExpr` remains honest on
  `runPipelineElab` and `runPipelineElabChecked` only via raising the
  terminal `src/MLF/Elab/TermClosure.hs`
  `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
  from `1` to `2` while keeping the outer
  `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
  retained-child route / guard cluster, pipeline facades, and fallback seams
  unchanged
- Accepted `round-188` settles one further adjacent same-lane packet only:
  `sameLaneSeptupleAliasFrameClearBoundaryExpr` remains honest on
  `runPipelineElab` and `runPipelineElabChecked` only via raising the
  terminal `src/MLF/Elab/TermClosure.hs`
  `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
  from `2` to `3` while keeping the outer
  `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
  retained-child route / guard cluster, pipeline facades, and fallback seams
  unchanged
- The current same-lane mechanism baseline is therefore explicit and bounded:
  `src/MLF/Elab/TermClosure.hs` still exposes
  `hasRetainedChildAliasBoundary v body 2 =`; the accepted septuple packet
  also depends on
  `hasRetainedChildClearBoundaryWithAliasBudget source term 3`, while the
  current guard baseline still forbids any explicit
  `hasRetainedChildAliasBoundary v body 3 =` or
  `hasRetainedChildClearBoundaryWithAliasBudget source term 4`
  entrypoint marker
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and the
  matching assertions in `test/PipelineSpec.hs` currently define and assert
  exactly seven same-lane retained-child packets:
  `sameLaneAliasFrameClearBoundaryExpr`,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
  `sameLaneSextupleAliasFrameClearBoundaryExpr`, and
  `sameLaneSeptupleAliasFrameClearBoundaryExpr`; the next deeper octuple
  packet has only fresh review-time probes so far, and those probes still
  fail closed on both authoritative entrypoints
- The admitted retained-child route stays fixed to
  `sameLaneLocalRetainedChildTarget` plus the
  `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard cluster; no new
  owner, binder, search, fallback, or interface family is admitted
- The settled alias, double-alias, triple-alias, quadruple-alias,
  quintuple-alias, sextuple-alias, and septuple-alias packets remain
  predecessor evidence only for this round, and the `P5` clear-boundary /
  nested-`forall` contrast pair plus the `C1` non-local packet remain
  adjacent control or predecessor evidence only
- Repository status in the canonical round worktree before writing this
  selection was `## orchestrator/round-189-bounded-positive-family-slice`
  with one pre-existing controller-owned modification:
  `M orchestrator/state.json`
- Controller-prepared round branch:
  `orchestrator/round-189-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-189`

## Scope

- Keep `item-5` active, but bind this round to exactly one bounded same-lane
  retained-child positive-family slice:
  `sameLaneOctupleAliasFrameClearBoundaryExpr` only
- Define that exact packet as the current
  `sameLaneSeptupleAliasFrameClearBoundaryExpr` shape plus one additional
  same-lane clear-boundary local alias binder between `tip` and the final
  retained-child consumer, for example
  `ELet "bud" (EVar "tip") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u"))`;
  do not introduce quantified crossing, a new route family, or a
  non-local / fallback detour
- Treat the accepted alias, double-alias, triple-alias, quadruple-alias,
  quintuple-alias, sextuple-alias, and septuple-alias packets honestly as
  baseline only; this round must determine whether the authoritative
  entrypoints preserve recursive output one alias shell beyond the accepted
  septuple case, or whether the current bounded helper honestly stops there
- Center any implementation work on the smallest current seams implicated by
  that question only: `src/MLF/Elab/TermClosure.hs`, plus the matching exact
  packet assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
- Reuse focused evidence only from the exact retained-child anchors already in
  place: the alias, double-alias, triple-alias, quadruple-alias,
  quintuple-alias, sextuple-alias, and septuple-alias coverage in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`; keep the `P5` clear-boundary / nested-`forall`
  pair and the `C1` non-local packet as predecessor or adjacent control
  evidence only
- Permit only the minimum source/test changes justified by that exact packet
  and the alias-boundary preservation seam; do not reopen general same-lane
  family settlement, do not alter the item-3 fail-closed
  route/ambiguity/termination contract, and do not widen the pipeline or
  fallback surfaces in place
- End with one honest read of this exact octuple-alias packet against the
  item-4 reconstruction-visible contract only; do not translate one packet
  into general `P3`, `P4`, or `P6` closure, and do not claim repo-level
  readiness
- Keep the round bounded and non-widening: no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, no new test module or Cabal wiring, and no roadmap/controller-state
  edits
