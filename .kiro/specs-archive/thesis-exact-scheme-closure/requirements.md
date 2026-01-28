# Requirements Document

## Introduction
Make scheme emission thesis-exact by removing the free-var closure fallback. Instead, ensure the binding structure already makes all named variables part of Γ at the point we emit a closed scheme, and fail fast when it does not. Out of scope: changes to presolution/solve or to binding generation beyond adding validations and error reporting.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want generalization to reject schemes with free vars, so that emitted schemes are thesis-exact and closed by construction.

#### Acceptance Criteria
1. WHEN `generalizeAt` emits a scheme THEN THE SYSTEM SHALL fail if the resulting type contains free type variables not bound by the scheme’s quantifiers.
2. WHEN the failure occurs THEN THE SYSTEM SHALL report a structured error that identifies the free variable(s) and the scheme root/scope.
3. WHERE the scheme is closed THEN THE SYSTEM SHALL preserve current behavior and name substitution.

### Requirement 2
**User Story:** As a translator, I want binding structure invariants that guarantee closed schemes without fallback, so that Q(g) is the sole source of bound variables.

#### Acceptance Criteria
1. WHEN validating a binding tree for scheme emission THEN THE SYSTEM SHALL detect any named node reachable from a scheme root that is not bound under the scheme’s gen node.
2. IF the invariant is violated THEN THE SYSTEM SHALL report a structured binding error with the offending scheme root, gen node, and free nodes.
3. WHEN the invariant holds THEN THE SYSTEM SHALL allow scheme emission without inserting additional binders.

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in thesis-exact behavior, so that regressions are caught.

#### Acceptance Criteria
1. WHEN running tests THEN THE SYSTEM SHALL include a negative test that previously relied on free-var closure and now fails with the new error.
2. WHEN running tests THEN THE SYSTEM SHALL keep existing thesis-exact Q(g)/Q(n) tests passing without extra binders.
