# Requirements Document

## Introduction
Chapter 15.2.6-15.2.7 describes alternative let scoping and a notion of
translatable presolutions. The current pipeline fixes a single scoping choice
and does not explicitly validate translatability. This spec captures the work
needed to align let scoping and presolution validity with the thesis.

## In Scope
- Represent and implement the alternative let-scoping choice described in the
  thesis.
- Validate or enforce translatable presolutions for lets.
- Add tests that show the alternative scoping choice is respected.

## Out of Scope
- Reworking the entire constraint generation pipeline.
- Changing witness normalization rules beyond what translatability requires.
- New optimization passes.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want let scoping to match the thesis so
quantifier placement is paper-aligned.

#### Acceptance Criteria
1. WHEN translating a let-binding THEN THE SYSTEM SHALL implement the
   alternative scoping rule from Chapter 15.2.6.
2. WHEN the alternative scoping choice affects quantification THEN THE SYSTEM
   SHALL reflect that difference in the elaborated scheme.

### Requirement 2
**User Story:** As a maintainer, I want presolutions to be translatable so
translation does not fail unexpectedly.

#### Acceptance Criteria
1. WHEN generating presolutions for let-bindings THEN THE SYSTEM SHALL verify
   the translatability conditions from Chapter 15.2.7.
2. WHEN a presolution is not translatable THEN THE SYSTEM SHALL report a
   structured error and avoid producing a partial translation.

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in the new scoping.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include regression tests
   for let-bound terms that differ under the alternative scoping.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a test that
   exercises translatable-presolution validation for lets.
