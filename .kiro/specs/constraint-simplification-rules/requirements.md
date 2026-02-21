# Requirements Document

## Introduction
Chapter 13 defines constraint simplification rules that reduce constraints while
preserving their solutions. The current implementation applies simplification
implicitly during presolution but does not expose the individual rules as
first-class operations aligned with the thesis definitions.

## In Scope
- Define the thesis simplification rules (Chapter 13) as explicit operations.
- Implement rule application with structured before/after constraint tracking.
- Add tests that validate each rule preserves constraint solutions.

## Out of Scope
- Changing the presolution algorithm beyond rule alignment.
- Performance optimization of simplification.
- Solver changes unrelated to simplification rules.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want constraint simplification rules to be
explicit and thesis-aligned so each rule can be tested independently.

#### Acceptance Criteria
1. WHEN applying a simplification rule THEN THE SYSTEM SHALL produce a
   simplified constraint that preserves solutions per Chapter 13.
2. WHEN a rule is not applicable THEN THE SYSTEM SHALL return the constraint
   unchanged.

### Requirement 2
**User Story:** As a reviewer, I want tests that lock in each simplification rule.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include a test for each
   simplification rule showing solution preservation.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a negative test
   showing a rule does not apply to an incompatible constraint.
