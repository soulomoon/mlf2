# Requirements Document

## Introduction
The thesis (section 15.2.4) defines a leftmost-lowermost ordering (<P) on
nodes used to order bounds and guide translation decisions. The current
implementation computes an order key, but it has not been explicitly audited
against the thesis definition. This spec requires a strict alignment.

## In Scope
- Implement or adjust the leftmost-lowermost ordering to match the thesis
  definition for paths and shared nodes.
- Ensure translation and witness normalization use the aligned ordering.
- Add tests that check ordering behavior on representative DAGs.

## Out of Scope
- Rewriting witness generation semantics.
- Changes to the overall constraint or presolution pipeline.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want the order keys to match the thesis so
ordering decisions are paper-aligned.

#### Acceptance Criteria
1. WHEN computing order keys for a root THEN THE SYSTEM SHALL select the
   leftmost-lowermost occurrence for each node in the term DAG, per section
   15.2.4.
2. WHEN a node is reachable via multiple paths THEN THE SYSTEM SHALL choose
   the best path according to the thesis ordering (<P) and keep results
   deterministic.

### Requirement 2
**User Story:** As a translator, I want Phi translation and merge ordering to
use the thesis order so binder placement is correct.

#### Acceptance Criteria
1. WHEN ordering binders for translation THEN THE SYSTEM SHALL use the
   leftmost-lowermost order for comparisons.
2. WHEN validating merge directions THEN THE SYSTEM SHALL use the same
   leftmost-lowermost order.

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in the ordering.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include regression tests
   that compare order keys for a branching DAG.
2. WHEN ordering is applied THEN THE SYSTEM SHALL include a property or unit
   test that validates stability for shared nodes.
