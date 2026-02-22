# Design Document

## Overview
Implement generalized unification for graphic types (Chapter 7.6). This extends
existing unification in presolution/solve with additional cases required for
thesis alignment.

## Architecture
- Introduce a helper module or extend existing unification logic with the
  generalized rules.
- Keep the current unification path as a fallback when rules do not apply.
- Add tests that exercise the new cases and error paths.

## Components and Interfaces
- `src/MLF/Constraint/Presolution/Unify.hs`
  - Generalized unification entry point.
- `src/MLF/Constraint/Unify/Decompose.hs`
  - Additional decomposition or rule handling.
- `src/MLF/Constraint/Types.hs`
  - Any supporting data structures or error types.

## Error Handling
- Add a structured error for generalized unification failures.

## Testing Strategy
- Add targeted unit tests for generalized cases.
- Add a regression test that differentiates generalized behavior from standard
  unification.

## References
- `papers/these-finale-english.txt` Chapter 7.6
