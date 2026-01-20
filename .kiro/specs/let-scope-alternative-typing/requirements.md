# Requirements Document

## Introduction
Chapter 15.2.6–15.2.7 describes an alternative let constraint (rightmost
constraint of Fig. 15.2.6), explains why the trivial let-scheme edge translates
to the identity computation (15.2.6.1), and defines "translatable presolutions"
(Def. 15.2.10). Our pipeline still uses the leftmost constraint (let shares the
body gen node) and does not explicitly validate translatability. This spec
aligns let scoping and presolution validity with the thesis.

## In Scope
- Implement the alternative let-scoping choice described in the thesis
  (rightmost constraint in Fig. 15.2.6).
- Validate or enforce translatable presolutions for lets (Def. 15.2.10).
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
   alternative scoping rule from Chapter 15.2.6 (rightmost constraint of
   Fig. 15.2.6).
2. WHEN building the constraint for `let x = a in b` THEN THE SYSTEM SHALL
   introduce a dedicated gen node for the let expression, bind a trivial
   scheme root (fresh type variable) under it, and type `b` under a fresh
   gen node that is a child of the let-expression gen node.
3. WHEN the alternative scoping choice affects quantification THEN THE SYSTEM
   SHALL reflect that difference in the elaborated scheme.
4. WHEN a trivial let-expression scheme is introduced THEN THE SYSTEM SHALL
   translate its instantiation edge to the identity computation for principal
   presolutions (15.2.6.1), avoiding extra term-level wrappers.

### Requirement 2
**User Story:** As a maintainer, I want presolutions to be translatable so
translation does not fail unexpectedly.

#### Acceptance Criteria
1. WHEN generating presolutions for let-bindings THEN THE SYSTEM SHALL enforce
   the translatability conditions from Chapter 15.2.7 (Def. 15.2.10):
   (1) no inert-locked nodes, (2) non-degenerate scheme roots rigid
   (scheme roots still bound on their gen node; Def. 10.1.1),
   (3) application/abstraction arrow nodes rigid,
   (4) non-interior nodes bound on gen nodes rigid.
2. WHEN translatability cannot be enforced THEN THE SYSTEM SHALL report a
   structured error and avoid producing a partial translation.
3. WHEN translatability enforcement still yields violations THEN THE error
   SHALL mention which condition (1–4) is violated (via per-condition issue tags).

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in the new scoping.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include regression tests
   for let-bound terms that differ under the alternative scoping.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a test that
   exercises translatable-presolution validation for lets.
