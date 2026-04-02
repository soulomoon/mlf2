# Round 174 — Task Selection

**Selected item**: item-2
**Item title**: Implement and validate one bounded current-architecture slice on the frozen representative-gap packet
**Roadmap identity**:
- roadmap_id: 2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap/rev-001
- roadmap_item_id: item-2

## Why now

`item-1` is now done, and no live retry state forces a same-round retry.

Accepted `round-173` already froze the exact next packet
`sameLaneDoubleAliasFrameClearBoundaryExpr`, the current exact live read for
that packet, the bounded item-2 success bar, and the fail-closed writable
slice. The next lawful move is therefore the roadmap's explicit item-2
implementation gate, not another freeze artifact and not any widened
architecture work.

The roadmap keeps item-2 exact-packet-only: it must stay inside the frozen
packet, the inherited explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary, and the fail-closed writable slice
centered on `src/MLF/Elab/TermClosure.hs` plus focused packet tests.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `9754887`
  (`Advance current-architecture follow-on roadmap after round-173`)
- Active roadmap revision: `rev-001`, with item `1` done and items `2`
  through `4` pending
- Live retry state: none recorded for this family
- Frozen exact packet:
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
- Current exact live read:
  both `runPipelineElab` and `runPipelineElabChecked` currently render the
  same top-level blocker
  `PipelineTypeCheckError (TCLetTypeMismatch ...)`
- Frozen writable slice:
  `src/MLF/Elab/TermClosure.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- Broader `P3` / `P4` / `P6` and repo-level readiness remain unresolved
- Controller-prepared round branch:
  `orchestrator/round-174-implement-double-alias-packet`
- Controller-prepared round worktree:
  `orchestrator/worktrees/round-174`

## Scope

- Implement exactly one bounded code-and-test slice on the frozen packet
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
- Stay inside the frozen writable slice and inherited current architecture
- Refresh focused verification honestly for the exact packet
- Record one exact-packet outcome only:
  `narrow success`,
  `fail-closed`, or
  `narrower current-architecture blocker`
- Do not widen into general `P3` / `P4` / `P6` settlement, repo-level
  readiness, cyclic search, multi-SCC search, fallback widening, equi-recursive
  reasoning, or second-interface work
