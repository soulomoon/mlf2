# Design Document

## Overview
Implement the alternative let scoping and translatable presolution checks
outlined in Chapter 15.2.6-15.2.7. The goal is to align how let-bound schemes
are scoped and ensure that the resulting presolutions can be translated to
xMLF without ad-hoc fixes.

## Architecture
- Extend constraint generation or presolution to encode the alternative scoping
  decision for lets.
- Add a validation pass (or reuse existing checks) to ensure presolutions are
  translatable under the chosen scoping.
- Update elaboration/generalization to consume the revised scoping information.

## Components and Interfaces
- `src/MLF/Frontend/ConstraintGen/*`
  - Let-binding translation and scoping decisions.
- `src/MLF/Constraint/Presolution/*`
  - Translatability checks for presolutions.
- `src/MLF/Elab/Generalize.hs`
  - Generalization scope selection for let bindings.

## Error Handling
- Introduce a specific error when presolutions are not translatable so the
  failure is explicit and diagnosable.

## Testing Strategy
- Add regression tests with nested lets and polymorphic uses that exercise the
  alternative scoping rule.
- Add a test that fails when presolution translatability is violated.

## References
- `papers/these-finale-english.txt` Chapter 15.2.6-15.2.7
