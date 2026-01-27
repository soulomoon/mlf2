# Duplicate Logic Candidates

This list captures duplicated logic that could be centralized for clarity and maintenance. Line numbers are approximate and should be re-checked when working on the item.

## Resolved (2026-01-27)
- Base-bound resolution: `expansionToInst` now uses `resolveBaseBoundForInstConstraint`, and bound-chain walking is centralized via `resolveBoundBodyConstraint`.
- Scope-aware bound/alias inlining: `inlineBoundVarsTypeWith` and pipeline `inlineAllBoundsType` now delegate to `inlineAliasBoundsWithBy`/`inlineAliasBoundsWithBySeen`.
