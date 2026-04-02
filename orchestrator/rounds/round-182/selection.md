# Round 182 — Task Selection

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

Accepted `round-181` already consumed the first bounded item-5 slice on the
exact `C1` non-local scheme-alias / base-like packet and made that baseline
more honest by removing the packet-local
`preserveC1AuthoritativeRecursiveAlias` /
`isBlockedC1AliasScheme` shortcut from `src/MLF/Elab/Run/Pipeline.hs` while
the authoritative entrypoints stayed recursively reconstruction-visible. That
settles one bounded `P2` packet only; it does not yet say whether the next
positive-family fragments rise above packet-specific folklore.

The next honest move is therefore to stay on item-5 and move to the lowest
remaining retained-child positive fragment already frozen by predecessor
authority: `sameLaneAliasFrameClearBoundaryExpr`. The accepted mechanism map
still records `preserveRetainedChildAuthoritativeResult` in
`src/MLF/Elab/TermClosure.hs` as a packet-local authoritative-preservation
seam, so the next bounded slice should determine whether this first settled
same-lane `P3` / `P4` / `P6` packet still preserves recursive authoritative
output without leaning on that packet-local rescue story.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `7daf8f1`
  (`Advance full-inference roadmap after round-181`)
- Active roadmap revision: `rev-001`, with items `1` through `4` done and
  items `5` through `7` pending
- Live retry state: none recorded for this family (`retry: null`)
- Accepted family docs from rounds `177` through `180` now control the round:
  predecessor authority freeze, mechanism map, fail-closed
  candidate/ambiguity/termination contract, and reconstruction-visible
  readiness contract are all in place
- Accepted `round-181` settles the first bounded item-5 `P2` slice only: the
  exact `C1` packet in `test/Research/C1AuthoritativeSurfaceSpec.hs` and
  `test/PipelineSpec.hs` no longer depends on the packet-local
  `Run/Pipeline` shortcut, while `runPipelineElab` and
  `runPipelineElabChecked` still return recursive output for that one packet
- `sameLaneAliasFrameClearBoundaryExpr` is already accepted predecessor truth
  only: `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
  records it as one bounded `narrow success` packet on
  `runPipelineElab` and `runPipelineElabChecked`, not as general `P3` / `P4`
  / `P6` closure
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and the
  matching assertions in `test/PipelineSpec.hs` already keep
  `sameLaneAliasFrameClearBoundaryExpr` review-visible on both authoritative
  entrypoints, while `sameLaneDoubleAliasFrameClearBoundaryExpr` remains an
  adjacent accepted control packet only
- The accepted mechanism map still names
  `preserveRetainedChildAuthoritativeResult` in
  `src/MLF/Elab/TermClosure.hs` as packet-local debt rather than a general
  retained-child rule
- The item-3-admitted retained-child route and guard contract remains fixed
  to `sameLaneLocalRetainedChildTarget` plus the
  `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard cluster; no
  new owner, binder, or search family is admitted
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-182-bounded-positive-family-slice`
- Controller-prepared round worktree: `orchestrator/worktrees/round-182`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round

## Scope

- Keep item-5 active, but bind this round to exactly one bounded retained-child
  positive-family slice: `sameLaneAliasFrameClearBoundaryExpr` only
- Treat the accepted predecessor narrow-success read honestly as baseline only;
  this round must determine whether that exact packet still preserves
  recursive authoritative output when the packet-local
  `preserveRetainedChildAuthoritativeResult` seam is challenged, narrowed, or
  shown unnecessary
- Center any implementation work on the smallest current seams implicated by
  that question only: `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`, and the matching authoritative facades only
  if those entrypoints require synchronization
- Reuse focused evidence only from the exact retained-child anchors already
  frozen by item-4: `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  and the matching `sameLaneAliasFrameClearBoundaryExpr` coverage in
  `test/PipelineSpec.hs`; keep `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  the clear-boundary `P5` control, the nested-`forall` contrast, and the `C1`
  non-local packet as adjacent control or predecessor evidence only
- Permit only the minimum source/test changes justified by that exact packet
  and that exact seam; do not add a new route family, do not reopen general
  same-lane family settlement, and do not alter the item-3 fail-closed
  candidate-generation / ambiguity / termination contract
- End with one honest read of this exact retained-child packet against the
  item-4 reconstruction-visible contract only; do not translate one packet
  into general `P3`, `P4`, or `P6` closure, and do not claim repo-level
  readiness
- Keep the round bounded and non-widening: no cyclic search, no multi-SCC
  handling, no equi-recursive reasoning, no fallback widening, no second
  interface, and no roadmap/controller-state edits
