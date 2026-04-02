# Round 178 — Task Selection

**Selected item**: item-2
**Item title**: Publish the current-architecture semantic mechanism map for automatic iso-recursive inference
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-2

## Why now

`item-2` is the lowest-numbered unfinished roadmap item, its dependency
`item-1` is done, and no live retry state forces a same-round retry.

Accepted `round-177` already froze the predecessor authority chain, the
unresolved semantic matrix, the family success bar, and the first concrete
deliverable for this full-inference family. That accepted item-1 result
explicitly carried forward `P2` through `P6` plus `N1`, `N2`, and `N6` as the
still-live obligations inside the inherited explicit-only / iso-recursive /
non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback current
architecture, while preserving `N3` through `N5` as out of scope unless a
later accepted decision revises them.

The next honest move is therefore the roadmap's reviewable mechanism-map
artifact, not item-3 search design, not item-4 reconstruction-contract work,
and not an implementation campaign. Item-2 is the first place this family may
explain, in one docs-only artifact, how the current implementation does or
does not support recursive-shape discovery, non-local propagation,
owner-sensitive placement, binder-sensitive placement, polymorphism /
nested-`forall` interaction, and reconstruction visibility without collapsing
narrow packet truth into an unwarranted repo-level readiness claim.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `2defaf3`
  (`Advance full-inference roadmap after round-177`)
- Active roadmap revision: `rev-001`, with item `1` done and items `2`
  through `7` pending
- Live retry state: none recorded for this family
- Accepted predecessor freeze: `round-177` completed item `1` and bound the
  first concrete deliverable to one docs-only current-architecture semantic
  mechanism map
- Accepted item-1 freeze artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
- Named bounded predecessor truth carried forward: both
  `sameLaneAliasFrameClearBoundaryExpr` and
  `sameLaneDoubleAliasFrameClearBoundaryExpr` remain accepted `narrow success`
  packets only; they do not settle `P2` through `P6` or repo-level
  full-inference readiness
- Still-live semantic obligations carried forward from item `1`: `P2`
  non-local propagation, `P3` owner-sensitive placement, `P4`
  binder-sensitive placement, `P5` polymorphism / nested-`forall`, `P6`
  reconstruction-visible output, plus `N1`, `N2`, and `N6`
- Out-of-scope negative families remain unchanged: `N3`
  equi-recursive-required, `N4` cyclic-or-multi-SCC-required, and `N5`
  second-interface-or-fallback-required
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-178-publish-semantic-mechanism-map`
- Controller-prepared round worktree: `orchestrator/worktrees/round-178`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round
- No accepted artifact in this family yet publishes one current-architecture
  mechanism map that separates settled predecessor fragments from still-missing
  general rules and names the smallest lawful code-facing seams for later
  bounded work

## Scope

- Author one docs-only item-2 mechanism-map artifact at
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- Explain, in semantic terms, how the current implementation does or does not
  support recursive-shape discovery, non-local propagation, owner-sensitive
  placement, binder-sensitive placement, polymorphism / nested-`forall`
  interaction, and reconstruction visibility
- Distinguish settled predecessor fragments from still-missing general rules
  and packet-specific handling that remains below a general account
- Identify the smallest lawful code-facing seams that later bounded rounds may
  touch without claiming full readiness or revising the inherited boundary
- Keep the round docs-only and non-widening: no production/test/Cabal edits,
  no roadmap or controller-state edits, no implementation plan, no search or
  termination contract finalization, and no repo-level readiness or
  boundary-revision claim
