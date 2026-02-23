# Design Document

## Overview
Implement similarity and abstraction relations for constraints (Chapter 13).
This adds explicit comparison and normalization logic so constraints can be
reasoned about up to similarity, as described in the thesis.

## Architecture
- Introduce a small module for similarity/abstraction utilities.
- Provide normalization functions that canonicalize constraints under
  similarity steps.
- Integrate these checks where constraints are compared or validated.

## Components and Interfaces
- `src/MLF/Constraint/Similarity.hs` (new)
  - Similarity and abstraction relations.
- `src/MLF/Constraint/Types.hs`
  - Data structures for abstraction metadata if needed.
- `src/MLF/Constraint/Solve.hs`
  - Optional hooks for similarity-aware validation.

## Error Handling
- Expose a structured error when similarity comparison fails due to malformed
  constraints.

## Testing Strategy
- Add unit tests for similarity equivalence and abstraction mapping.
- Add regression tests for canonicalization stability.

## References
- `papers/these-finale-english.txt` Chapter 13
