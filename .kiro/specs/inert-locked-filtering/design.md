# Design Document

## Overview
Thesis section 15.2.3 requires that propagation witnesses never transform
inert-locked nodes. These nodes cannot be represented in xMLF translation,
so we must either rewrite the presolution (by weakening inert nodes) or reject
witnesses that touch them. This spec implements detection, presolution
weakening, and witness filtering.

## Architecture
- **Inert classification**
  - Add a helper (new module `MLF.Constraint.Inert` or similar) to compute
    the set of inert nodes and inert-locked nodes from a `Constraint`.
  - Inert-locked definition: `n` is inert, is flexibly bound, and its
    binding path contains a rigid edge (thesis 15.2.2).
- **Presolution rewrite**
  - Add a pass in presolution/solve that weakens inert-locked nodes to rigidly
    bound nodes when possible (Lemma 15.2.4), producing an inert-equivalent
    presolution.
  - Record a structured error if the rewrite cannot produce a translatable
    presolution.
- **Witness filtering**
  - Extend `OmegaNormalizeEnv` with an `inertLocked` set and filter any
    operations that target inert-locked nodes before validation/translation.
  - Ensure `normalizeInstanceStepsFull` and Phi translation only see filtered
    witnesses.

## Components and Interfaces
- New helper module: `src/MLF/Constraint/Inert.hs` (proposed)
  - `inertNodes :: Constraint -> Either BindingError IntSet`
  - `inertLockedNodes :: Constraint -> Either BindingError IntSet`
- Presolution/solve hook:
  - `src/MLF/Constraint/Presolution/Driver.hs` or `src/MLF/Constraint/Solve.hs`
    to run the inert-locked weakening pass before witness translation.
- Witness normalization:
  - `src/MLF/Constraint/Presolution/Witness.hs` to drop ops touching
    inert-locked nodes.

## Data Models
- Add an `inertLocked :: IntSet` field to `OmegaNormalizeEnv` (or a separate
  environment record) so normalization and translation can filter operations.
- Add a new `PresolutionError` or `ElabError` constructor for inert-locked
  violations if rewriting fails.

## Error Handling
- If inert-locked nodes remain after rewriting, return a structured error and
  do not attempt translation.
- If inert detection fails due to binding tree invariants, surface
  `BindingTreeError`.

## Testing Strategy
- **Regression test**: construct a constraint with an inert-locked node and
  verify that operations targeting it are removed or rejected.
- **Property test**: for any normalized witness, no op target is in the
  inert-locked set.
- **Integration test**: translation succeeds for a case that previously
  contained inert-locked operations after the rewrite pass.

## References
- `papers/these-finale-english.txt` section 15.2.3 (inert-locked nodes)
- Lemma 15.2.4 (weakening inert nodes)
