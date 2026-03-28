# Merge Notes — round-140 / item-2

## Squash commit title

`item-2: Wire automatic μ-introduction tests and fix TyMu reification regression`

## Summary

Added focused pipeline coverage for automatic μ-introduction and aligned the implementation with the new recursive inference expectations at the authoritative entrypoints. The round also fixes a TyMu reification regression so the failure mode matches the expected translatability blocker path.

## Files changed

- `src/MLF/Constraint/Acyclicity.hs`
- `src/MLF/Constraint/Solved/Internal.hs`
- `src/MLF/Elab/Elaborate/Algebra.hs`
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- `src/MLF/Reify/Type.hs`
- `test/PipelineSpec.hs`

## Test evidence

- `cabal build all && cabal test`
- Result: `1162 examples, 0 failures`

## Follow-up notes

Remaining roadmap items: 3, 4, 5, and 6. These stay open for later rounds; no additional merge blocker was identified in this item.
