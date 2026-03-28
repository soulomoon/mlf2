# Round 125 Selection

Date: 2026-03-28
Round: `round-125`
Role: guider
Active subject: bounded `P1` local-recursive-shape authoritative-surface lane

## Roadmap Identity

- Roadmap ID:
  `2026-03-28-01-p1-local-recursive-shape-authoritative-surface-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-28-01-p1-local-recursive-shape-authoritative-surface-successor-roadmap/rev-001`
- Selected Item ID: `item-2`

## Selected Task

Roadmap item `2`: implement and validate one bounded current-architecture
`P1` authoritative-surface continuation slice.

## Why This Runs Now

Item `1` is already accepted and marked done, `retry` is `null`, and item `2`
is the lowest-numbered unfinished item in the active roadmap. This makes
item `2` the next lawful move and the first code-bearing round in this
bounded `P1` successor family.

## Frozen Predecessor Authority From Item `1`

The governing predecessor authority for this round is the accepted
round-124 freeze artifact:

`docs/plans/2026-03-28-p1-local-recursive-shape-successor-authority-success-bar-and-writable-slice-freeze.md`

That freeze binds this round to all of the following:

- exact live subject only:
  `P1 local-recursive-shape`
- exact packet only:
  the corresponding unannotated variant `ELam "x" (EVar "x")`
- exact inherited contrast only:
  the same-lane local `TypeRef` control lane remains the positive reference,
  while the exact unannotated packet still yields `containsMu False` on the
  current authoritative pipeline surfaces in `test/PipelineSpec.hs`
- exact success bar only:
  recursive structure must become review-visible on the current authoritative
  surfaces for that exact unannotated local packet without widening
  architecture or interface boundaries
- exact inherited boundary only:
  explicit recursive annotations remain the production baseline, recursive
  meaning remains iso-recursive only, `non-equi-recursive = keep`, the
  inherited non-cyclic structural boundary remains binding, `no-fallback =
  keep`, and one-interface-only remains binding

## Bounded Scope For The First Code-Bearing Round

This round is bounded to one exact-packet implementation-and-validation slice
only. It may work only inside the item-1 frozen writable slice:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/PipelineSpec.hs`
- `test/Research/P1LocalRecursiveShapeSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- round-owned round-125 artifacts and the item-1 freeze doc if a bounded
  authority note is required

The round must stay concrete:

- preserve the exact frozen live packet `ELam "x" (EVar "x")`
- target authoritative-surface continuity for that packet only
- use the existing same-lane local `TypeRef` control only as inherited
  contrast, not as a second live lane
- refresh focused regression evidence for this exact packet
- rerun the full repo gate if code paths under `src/`, `src-public/`, `app/`,
  `test/`, or `mlf2.cabal` are touched, unless a later review records a
  contract-allowed exception

The round must not widen into:

- the settled `C1` / `P2` packet
- the settled same-lane `C2` / `C5` / `C7` pocket
- `P5` as a second live lane
- cyclic search
- multi-SCC search
- equi-recursive reasoning
- fallback widening
- a second interface
- repo-level readiness or capability claims

## Parallel-Lane Statement

This roadmap item remains one controller-selected round and is not
aggregate-only, but no default worker fan-out is authorized here. Any
parallel sidecars inside the round would need to stay subordinate to the one
selected item, use disjoint write scopes, and preserve one authoritative
round-owned output set.
