# Design Document

## Overview
Implement the constraint simplification rules from Chapter 13 as explicit,
testable operations. These rules reduce constraints while preserving their
solution sets, and are used during presolution to normalize constraint graphs.

## Architecture
- Introduce a module for individual simplification rules.
- Each rule is a function from Constraint to Maybe Constraint (Nothing if
  not applicable).
- Integrate rule application into the presolution pipeline where implicit
  simplification currently occurs.

## Components and Interfaces
- `src/MLF/Constraint/Simplification.hs` (new)
  - Individual simplification rules per Chapter 13.
  - Rule composition and fixed-point application.
- `src/MLF/Constraint/Presolution/Driver.hs`
  - Hook for explicit rule application during presolution.

## Error Handling
- Rules that detect malformed constraints emit structured errors.

## Testing Strategy
- Unit tests for each individual rule.
- Integration tests showing rule composition during presolution.
- Solution-preservation regression tests.

## References
- `papers/these-finale-english.txt` Chapter 13 (constraint simplification)
