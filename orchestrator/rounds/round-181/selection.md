# Round 181 — Task Selection

**Selected item**: item-5
**Item title**: Run the bounded positive-family implementation and evidence campaign
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-5

## Why now

`item-5` is the lowest-numbered unfinished roadmap item, its dependencies
`item-1` through `item-4` are done, and no live retry state forces a
same-round retry.

Accepted `round-177` through `round-180` already froze the predecessor
authority chain, published the current-architecture mechanism map, fixed the
fail-closed route/search contract, and defined the reconstruction-visible
readiness contract plus authoritative corpus surfaces. The family is therefore
at its first lawful implementation/evidence step, but still only one bounded
positive-family slice at a time.

The first slice should start with `P2 non-local-propagation`, because it is
the lowest-numbered still-live positive family and item-4 already bound one
exact representative authoritative-surface anchor for it: the admitted
non-local scheme-alias/base-like `C1` packet in
`test/Research/C1AuthoritativeSurfaceSpec.hs`. Starting with that exact packet
uses the already-admitted `rootNonLocalSchemeAliasBaseLike` route, tests the
still-live non-local obligation before reopening same-lane or polymorphic
families, and preserves the accepted same-lane wins as predecessor truth
rather than silently treating them as family closure.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `664bef7`
  (`Advance full-inference roadmap after round-180`)
- Active roadmap revision: `rev-001`, with items `1` through `4` done and
  items `5` through `7` pending
- Live retry state: none recorded for this family (`retry: null`)
- Accepted family docs from rounds `177` through `180` now control the round:
  predecessor authority freeze, mechanism map, fail-closed
  candidate/ambiguity/termination contract, and reconstruction-visible
  readiness contract are all in place
- The current admitted non-local route remains the item-3-bound
  `rootNonLocalSchemeAliasBaseLike` arm centered on `baseTarget -> baseC`; no
  other non-local route family is admitted
- `test/Research/C1AuthoritativeSurfaceSpec.hs` already carries the exact
  first `P2` packet:
  `c1Expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")`
- That harness already records the honest current split read: the fallback
  surface stays visibly non-recursive (`TBase Int`, `containsMu False`), while
  `runPipelineElab` and `runPipelineElabChecked` currently return recursive
  output (`containsMu True`) for the same exact packet
- Matching non-local assertions already exist in `test/PipelineSpec.hs`,
  keeping the `rootNonLocalSchemeAliasBaseLike` / `baseTarget -> baseC` lane
  and authoritative-surface recursive output review-visible
- No accepted round in this full-inference family yet consumes that exact `C1`
  baseline as item-5 positive-family evidence or states whether it rises above
  packet-specific folklore
- Accepted same-lane packets `sameLaneAliasFrameClearBoundaryExpr` and
  `sameLaneDoubleAliasFrameClearBoundaryExpr`, plus the clear-boundary `P5`
  control in `test/Research/P5ClearBoundarySpec.hs`, remain predecessor or
  adjacent control evidence only; they are not the live implementation target
  for this first item-5 slice
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-181-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-181`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round

## Scope

- Start item-5 with exactly one bounded first positive-family slice: the `P2`
  non-local-propagation `C1` packet only
- Keep the exact live packet fixed to the admitted non-local scheme-alias /
  base-like authoritative-surface harness in
  `test/Research/C1AuthoritativeSurfaceSpec.hs`, not the same-lane
  retained-child packets, not the nested-`forall` contrast, and not any
  broader family bundle
- Center any implementation work on the existing non-local route story only:
  `rootNonLocalSchemeAliasBaseLike`, `baseTarget -> baseC`, and
  authoritative-surface continuity through `runPipelineElab` and
  `runPipelineElabChecked`
- Reuse focused evidence only from the exact `C1` anchors already frozen by
  item-4: `test/Research/C1AuthoritativeSurfaceSpec.hs` and the matching
  non-local assertions in `test/PipelineSpec.hs`; keep
  `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs` synchronized
  only if the authoritative surfaces themselves require it
- Permit only the minimum source/test changes justified by that exact packet
  and those exact surfaces; do not add a new route family, do not reopen
  same-lane retained-child routing, and do not alter the item-3 fail-closed
  candidate-generation / ambiguity / termination contract
- Preserve the fallback-surface non-recursive read honestly as supporting
  context only; success for this slice must still be judged on the item-4
  authoritative surfaces, not on fallback-only or helper-only behavior
- End with one honest read of this exact `P2` slice against the item-4
  reconstruction-visible contract only; do not translate one packet into
  general `P2`-`P6` closure or repo-level readiness
- Keep the round bounded and non-widening: no general `P2`-`P6` settlement
  claim, no repo-level readiness claim, no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, and no roadmap/controller-state edits
