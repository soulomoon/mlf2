# Round 126 Implementation Notes

Date: 2026-03-28
Round: `round-126`
Roadmap item: `item-3`
Attempt: `attempt-1`
Outcome branch: `exact post-item-2 P1 read republished`

## Summary

- Published one canonical settlement surface for the exact frozen packet
  `ELam "x" (EVar "x")` after the accepted round-125 fail-closed evidence.
- Kept the round docs-only and non-widening.
- Bound the artifact to the accepted item-1 freeze, round-125 implementation
  notes, round-125 review, round-125 review record, and the strengthened
  exact-packet regression in `test/PipelineSpec.hs`.

## Exact Read Republished

- The exact frozen packet remains `containsMu False` on:
  - the internal fallback route from `computeResultTypeFallback`; and
  - both authoritative entrypoints `runPipelineElab` and
    `runPipelineElabChecked`.
- The accepted round-125 route audit found no lawful recursive carrier for
  that packet inside the frozen writable slice.
- The inherited architecture remains unchanged.

## Files Changed

- `docs/plans/2026-03-28-post-implementation-p1-local-recursive-shape-settlement-surface-and-exact-repo-impact-read.md`
- `orchestrator/rounds/round-126/*`
