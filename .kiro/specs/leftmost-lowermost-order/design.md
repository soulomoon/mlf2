# Design Document

## Overview
Section 15.2.4 defines a leftmost-lowermost ordering (<P) on nodes based on
structural paths. This order drives binder ordering and merge direction
choices. The implementation already computes order keys, but this spec
requires an explicit alignment and tests for shared-node cases.

## Architecture
- **Ordering algorithm**
  - Audit `MLF.Util.OrderKey.orderKeysFromRootWith` against the thesis
    definition of <P (path-based, leftmost tie-break, lowermost preference).
  - If mismatched, update the algorithm and document the mapping.
- **Usage audit**
  - Ensure `MLF.Elab.Phi` and `MLF.Constraint.Presolution.Witness` use the
    aligned ordering consistently.
- **Determinism**
  - For shared nodes, ensure the chosen path is stable and deterministic.

## Components and Interfaces
- `src/MLF/Util/OrderKey.hs`
  - Core ordering algorithm and comparison function.
- `src/MLF/Util/Order.hs`
  - Public helpers used by translation and normalization.
- `src/MLF/Elab/Phi.hs`
  - Binder ordering and placement logic.
- `src/MLF/Constraint/Presolution/Witness.hs`
  - Merge direction validation and deterministic normalization.

## Data Models
- `OrderKey` may be extended if the thesis requires additional path metadata.
  Keep comparison semantics in a single helper to avoid divergence.

## Error Handling
- If an order key is missing for a referenced node, emit a structured error
  rather than falling back silently.

## Testing Strategy
- **Regression tests** for a branching DAG where leftmost and lowermost
  disagree, asserting the thesis order.
- **Shared-node test** where a node has multiple paths; ensure the chosen path
  is the best according to <P and remains deterministic.
- **Integration test** ensuring translation uses the aligned order.

## References
- `papers/these-finale-english.txt` section 15.2.4 (leftmost-lowermost order)
- `papers/xmlf.txt` section 3.4 (ordering used in normalization)
