# Round 170 — Task Selection

**Selected item**: item-2
**Item title**: Implement and validate one bounded current-architecture slice on the frozen blocker lane
**Roadmap identity**:
- roadmap_id: 2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001
- roadmap_item_id: item-2

## Why now

item-2 is the lowest-numbered unfinished roadmap item, its only dependency
`item-1` is now done, and no live retry state forces a same-round retry.

Accepted `round-169` froze the predecessor authority chain, the exact
inherited packet `sameLaneAliasFrameClearBoundaryExpr`, the current exact
blocker read `PipelineTypeCheckError (TCLetTypeMismatch ...)`, the exact
item-2 success bar, and the explicit current-architecture writable slice. The
next honest move is therefore the one bounded code-and-test slice authorized
by that freeze, not another docs-only aggregate restatement.

The roadmap makes this item exact-packet-only: it must stay inside the frozen
item-1 writable slice, refresh focused regression coverage for the inherited
lane, and record one bounded outcome only for that packet.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `f254725`
- Active roadmap revision: `rev-001`, with item `1` done and items `2`
  through `4` still pending
- Live retry state: none recorded for this family
- The inherited exact packet remains
  `sameLaneAliasFrameClearBoundaryExpr`, anchored by
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- The current exact blocker read on both `runPipelineElab` and
  `runPipelineElabChecked` remains
  `PipelineTypeCheckError (TCLetTypeMismatch ...)`
- The frozen writable slice remains limited to
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/Scope.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `test/PipelineSpec.hs`,
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
  `test/Main.hs`,
  `mlf2.cabal`,
  the canonical item-1 freeze artifact,
  and `orchestrator/rounds/round-170/*`

## Scope

- Implement exactly one bounded code-and-test slice on the frozen packet
  `sameLaneAliasFrameClearBoundaryExpr`
- Stay inside the item-1 writable slice and inherited explicit-only /
  iso-recursive / non-equi-recursive / non-cyclic / no-fallback boundary
- Refresh the focused regression coverage for the inherited lane honestly
- Run `cabal build all && cabal test` because this round is expected to touch
  code and/or tests
- Record one exact-packet outcome only:
  narrow success,
  fail-closed,
  or a narrower current-architecture blocker
- Do not widen into general `P3` / `P4` / `P6` settlement, repo-level
  readiness claims, cyclic search, multi-SCC search, fallback widening, or
  second-interface work
