# Design Document

## Overview
Implement the syntactic and graphic translation rules from Chapter 8, including
bound inlining. The primary goal is to make reification and internalization
explicitly match the thesis, with clear tests that exercise the rules.

## Architecture
- Audit `MLF.Elab.Reify` for current translation decisions and align them with
  Chapter 8.
- Ensure internalization preserves binder identities and bounds, possibly by
  adding helper functions in constraint generation.
- Add targeted tests for round-trip translation and bound inlining.

## Components and Interfaces
- `src/MLF/Elab/Reify.hs`
  - Graphic-to-syntactic translation and bound inlining.
- `src/MLF/Frontend/ConstraintGen/*`
  - Syntactic-to-graphic translation of annotated types/schemes.
- `src/MLF/Constraint/Types.hs`
  - Helper definitions for traversing graphic structures.

## Error Handling
- Reification should fail with explicit errors when required nodes are missing
  or malformed.

## Testing Strategy
- Add tests that reify known graphs and check the syntactic type.
- Add tests that internalize syntactic types and compare expected graph shape.

## References
- `papers/these-finale-english.txt` Chapter 8
