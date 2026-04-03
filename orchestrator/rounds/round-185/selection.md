# Round 185 — Task Selection

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

Accepted `round-181` through `round-184` settled four bounded `item-5`
packets only: one exact `P2` non-local packet plus three exact same-lane
retained-child packets. Accepted `round-184` tightened the current same-lane
read further:
`sameLaneTripleAliasFrameClearBoundaryExpr` remains honest on
`runPipelineElab` and `runPipelineElabChecked` only via an exact depth-2 /
two-extra-alias-shell `TermClosure` rule, with the entrypoint still locked to
`hasRetainedChildAliasBoundary v body 1 =` and no explicit depth-`2`
entrypoint marker admitted.

The next honest move is therefore to stay on `item-5` and challenge the
smallest fresh same-lane retained-child packet immediately beyond that now
accepted bounded rule:
`sameLaneQuadrupleAliasFrameClearBoundaryExpr`, defined as the current
triple-alias clear-boundary packet plus one more same-lane local alias binder
between `more` and the final retained-child consumer. No accepted artifact
yet names or settles that exact packet, so selecting it now preserves the
alias, double-alias, and triple-alias packets as predecessor truth only while
asking the next concrete bounded question the current repo baseline actually
leaves open.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `2f02c72`
  (`Advance full-inference roadmap after round-184`)
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
- The current same-lane mechanism baseline is therefore explicit and bounded:
  `src/MLF/Elab/TermClosure.hs` still exposes
  `hasRetainedChildAliasBoundary v body 1 =`; the accepted triple-alias
  packet now also depends on the exhausted-budget
  `hasRetainedChildClearBoundary` branch, while the current test baseline
  still forbids any explicit depth-`2` entrypoint marker
- No accepted artifact in this family yet records a fresh current-roadmap
  read for any deeper same-lane retained-child packet beyond the settled
  triple-alias case
- The admitted retained-child route stays fixed to
  `sameLaneLocalRetainedChildTarget` plus the
  `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard cluster; no new
  owner, binder, search, fallback, or interface family is admitted
- The settled alias, double-alias, and triple-alias packets remain
  predecessor evidence only for this round, and the `P5` clear-boundary /
  nested-`forall` contrast pair plus the `C1` non-local packet remain
  adjacent control or predecessor evidence only
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-185-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-185`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round

## Scope

- Keep `item-5` active, but bind this round to exactly one bounded same-lane
  retained-child positive-family slice:
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr` only
- Define that exact packet as the current
  `sameLaneTripleAliasFrameClearBoundaryExpr` shape plus one additional
  same-lane clear-boundary local alias binder between `more` and the final
  retained-child consumer, for example
  `ELet "deep" (EVar "more") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u"))`;
  do not introduce quantified crossing, a new route family, or a
  non-local/fallback detour
- Treat the accepted alias, double-alias, and triple-alias packets honestly
  as baseline only; this round must determine whether the authoritative
  entrypoints still preserve recursive output one alias shell beyond the now
  accepted triple-alias rule, or whether the current architecture honestly
  stops there
- Center any implementation work on the smallest current seams implicated by
  that question only: `src/MLF/Elab/TermClosure.hs`, plus the matching exact
  packet assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
- Reuse focused evidence only from the exact retained-child anchors already in
  place: the alias, double-alias, and triple-alias coverage in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`; keep the `P5` clear-boundary / nested-`forall`
  pair and the `C1` non-local packet as predecessor or adjacent control
  evidence only
- Permit only the minimum source/test changes justified by that exact packet
  and the alias-boundary preservation seam; do not reopen general same-lane
  family settlement, do not alter the item-3 fail-closed
  route/ambiguity/termination contract, and do not widen the pipeline or
  fallback surfaces in place
- End with one honest read of this exact quadruple-alias packet against the
  item-4 reconstruction-visible contract only; do not translate one packet
  into general `P3`, `P4`, or `P6` closure, and do not claim repo-level
  readiness
- Keep the round bounded and non-widening: no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, no new test module or Cabal wiring, and no roadmap/controller-state
  edits
