# Design Document

## Overview
Normalize witnesses per the paper’s conditions (1)–(5) without elimination
pruning in either the normalizer or emission. The witness sequence should be
governed only by the paper’s conditions.

## Architecture
- `normalizeInstanceOpsFull` remains the canonical checker/reorderer for
  conditions (1)–(5).
- Emission/integration logic no longer removes ops based on later eliminations.

## Components and Interfaces
- `MLF.Constraint.Presolution.Witness`
  - Remove elimination pruning from `normalizeInstanceOpsFull`.
  - Remove elimination-based filtering inside `integratePhase2Ops`.
- `MLF.Constraint.Presolution.Driver`
  - Use the emission helper before normalization.
- `MLF.Constraint.Presolution.EdgeUnify`
  - Ensure `recordEliminate` + emitted merges already minimize duplicate
    eliminations; keep emitting raises/merges as today.
-- Tests in `test/PresolutionSpec.hs`
  - Update the “drops redundant Graft/Weaken when a binder is eliminated” check
    to assert that normalization does **not** drop these ops.
  - Add a regression that `integratePhase2Ops` preserves Graft/Weaken even when
    a later Merge/RaiseMerge eliminates the same binder.

## Data Models
No data model changes. This is a behavioral shift to remove pruning entirely.

## Error Handling
Normalization errors remain unchanged. Emission no longer filters ops based on
later eliminations.

## Testing Strategy
- Unit test that `normalizeInstanceOpsFull` does not prune redundant ops when
  called directly.
- Regression test that `integratePhase2Ops` preserves Graft/Weaken even when a
  later Merge/RaiseMerge eliminates the same binder.
- Full test suite to ensure other phases remain intact.
