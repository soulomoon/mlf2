# Round 167 — Implementation Notes (item-8: Public API Enrichment)

## Summary

Enriched the public API surface of the MLF2 library with error-formatting
helpers in `MLF.Pipeline` and constraint-graph introspection types/helpers in
`MLF.API`.

## Changes

### `MLF.Pipeline` — New exports

| Export | Description |
|--------|-------------|
| `CycleError(..)` | Re-exported from `MLF.Constraint.Acyclicity` |
| `formatPipelineError` | Structured multi-line `Text` rendering of `PipelineError` |
| `pipelineErrorPhase` | Extract numeric pipeline phase (1–7) |
| `pipelineErrorPhaseName` | Human-readable phase name |

Three new functions are defined locally in `MLF.Pipeline`. They pattern-match
on all `PipelineError` constructors for totality under `-Wall`.

### `MLF.API` — New exports

| Export | Description |
|--------|-------------|
| `Constraint(..)` | Full constraint graph record |
| `NodeId(..)`, `TyNode(..)`, `InstEdge(..)`, `UnifyEdge(..)` | Core graph types |
| `GenNode(..)`, `GenNodeId(..)`, `BindFlag(..)` | Gen-node and binding types |
| `BindParents`, `NodeMap`, `GenNodeMap` | Type aliases |
| `lookupNode` | Node lookup in `NodeMap` |
| `constraintNodeCount` | Count of type nodes |
| `constraintEdgeCount` | Count of inst + unify edges |

`constraintNodeCount` and `constraintEdgeCount` are thin helpers defined locally
in `MLF.API`, delegating to `toListNode` and record field accessors.

### `mlf2.cabal`

Added `text` to `build-depends` for both the public library and test suite.

### Test changes

- `test/PublicSurfaceSpec.hs`: Added 6 new test cases covering all new exports
  (error formatting, phase numbering, phase naming, node count, edge count, and
  `lookupNode` retrieval).
- `test/ConstraintGenSpec.hs`: Added `hiding (lookupNode)` to the `MLF.API`
  import to resolve the ambiguity with the `SpecUtil` re-export.

## Design decisions

1. **Leading-comma style preserved**: `MLF.Pipeline` export list uses the
   original leading-comma style to satisfy the `RepoGuardSpec` guardrail test
   marker `"\n    , normalize\n"`.

2. **`lookupNode` clash**: Adding `lookupNode` to `MLF.API` caused ambiguity in
   `ConstraintGenSpec.hs` which imports both `MLF.API` and `SpecUtil` (the
   latter defines its own `lookupNode`). Resolved with `hiding`.

3. **No internal behavior changes**: All new code is pure surface-level API
   enrichment. No internal pipeline logic was modified.

## Verification

- `cabal build all` — clean, no warnings
- `cabal test` — 1302 examples, 0 failures
