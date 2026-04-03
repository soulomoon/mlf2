# Round 184 — Task Selection

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

Accepted `round-181` through `round-183` settled three bounded item-5 packets
only: one exact `P2` non-local packet plus two exact same-lane retained-child
packets. Accepted `round-183` then tightened the current same-lane read
further: `sameLaneDoubleAliasFrameClearBoundaryExpr` remains honest on
`runPipelineElab` and `runPipelineElabChecked` only via the already-existing
bounded one-extra-alias-shell `TermClosure` rule, and the current test
baseline now locks that mechanism to `hasRetainedChildAliasBoundary v body 1 =`
with no depth-`2` entrypoint.

The next honest move is therefore to stay on item-5 and challenge the
smallest fresh same-lane retained-child packet immediately beyond that now
locked bounded rule: `sameLaneTripleAliasFrameClearBoundaryExpr`, defined as
the current double-alias clear-boundary packet plus one more same-lane local
alias binder before the retained-child consumer. No accepted artifact yet
names or settles that exact packet, so selecting it now preserves the accepted
one-alias and double-alias packets as predecessor truth only while asking the
next concrete bounded question the current repo baseline actually leaves open.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `5d18029`
  (`Advance full-inference roadmap after round-183`)
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
- The current same-lane mechanism baseline is therefore explicit and bounded:
  `src/MLF/Elab/TermClosure.hs` still exposes
  `hasRetainedChildAliasBoundary v body 1 =`, and the current test baseline
  explicitly rejects any already-present depth-`2` entrypoint marker
- No accepted artifact in this family yet records a fresh current-roadmap read
  for any deeper same-lane retained-child packet beyond the settled
  double-alias case
- The admitted retained-child route stays fixed to
  `sameLaneLocalRetainedChildTarget` plus the
  `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard cluster; no new
  owner, binder, search, fallback, or interface family is admitted
- The settled one-alias and double-alias packets remain predecessor evidence
  only for this round, and the `P5` clear-boundary / nested-`forall`
  contrast pair plus the `C1` non-local packet remain adjacent control or
  predecessor evidence only
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-184-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-184`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round

## Scope

- Keep item-5 active, but bind this round to exactly one bounded same-lane
  retained-child positive-family slice:
  `sameLaneTripleAliasFrameClearBoundaryExpr` only
- Define that exact packet as the current
  `sameLaneDoubleAliasFrameClearBoundaryExpr` shape plus one additional
  same-lane clear-boundary local alias binder between `keep` and the final
  retained-child consumer; do not introduce quantified crossing, a new route
  family, or a non-local/fallback detour
- Treat the accepted one-alias and double-alias packets honestly as baseline
  only; this round must determine whether the authoritative entrypoints still
  preserve recursive output one alias shell beyond the now-locked bounded
  rule, or whether the current architecture honestly stops there
- Center any implementation work on the smallest current seams implicated by
  that question only: `src/MLF/Elab/TermClosure.hs`, plus the matching exact
  packet assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
- Reuse focused evidence only from the exact retained-child anchors already in
  place: the one-alias and double-alias coverage in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`; keep the `P5` clear-boundary / nested-`forall`
  pair and the `C1` non-local packet as predecessor or adjacent control
  evidence only
- Permit only the minimum source/test changes justified by that exact packet
  and the alias-boundary preservation seam; do not reopen general same-lane
  family settlement, do not alter the item-3 fail-closed
  route/ambiguity/termination contract, and do not widen the pipeline or
  fallback surfaces in place
- End with one honest read of this exact triple-alias packet against the
  item-4 reconstruction-visible contract only; do not translate one packet
  into general `P3`, `P4`, or `P6` closure, and do not claim repo-level
  readiness
- Keep the round bounded and non-widening: no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, no new test module or Cabal wiring, and no roadmap/controller-state
  edits
