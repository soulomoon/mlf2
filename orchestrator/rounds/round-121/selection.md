# Round 121 Selection

Date: 2026-03-28
Round: `round-121`
Role: guider
Active subject: bounded `C1` / `P2` authoritative-surface successor lane

## Roadmap Identity

- Roadmap ID:
  `2026-03-28-00-c1-p2-authoritative-surface-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-28-00-c1-p2-authoritative-surface-successor-roadmap/rev-001`
- Selected Item ID: `item-2`

## Selected Task

Roadmap item `2`: implement and validate one bounded current-architecture
`C1` authoritative-surface continuation slice.

## Why This Runs Now

Item `1` is already accepted and marked done, `retry` is `null`, and item `2`
is the lowest-numbered unfinished item in the active roadmap. This makes
item `2` the next lawful move and the first code-bearing round in this
successor family.

## Frozen Predecessor Authority From Item `1`

The governing predecessor authority for this round is the accepted
round-120 freeze artifact:

`docs/plans/2026-03-28-c1-p2-authoritative-surface-successor-authority-success-bar-and-writable-slice-freeze.md`

That freeze binds this round to all of the following:

- exact live subject only:
  `C1` under `P2 non-local-propagation`
- exact packet and route only:
  `baseTarget -> baseC -> targetC`
- exact inherited current read only:
  fallback stays `TBase (BaseTy "Int")` with `containsMu False`, and the
  current authoritative public pipeline entrypoints still surface the
  non-`TMu` `forall`-identity shape
- exact success bar only:
  recursive structure must become visible on the current authoritative
  surfaces for this exact admitted packet without widening architecture or
  interface boundaries
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
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- round-owned round-121 artifacts and the item-1 freeze doc if a bounded
  authority note is required

The round must stay concrete:

- preserve the exact `C1` packet on `baseTarget -> baseC -> targetC`
- target current authoritative-surface continuity only
- refresh focused regression evidence for this exact packet
- rerun the full repo gate if code paths under `src/`, `src-public/`, `app/`,
  `test/`, or `mlf2.cabal` are touched, unless a later review records a
  contract-allowed exception

The round must not widen into:

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
