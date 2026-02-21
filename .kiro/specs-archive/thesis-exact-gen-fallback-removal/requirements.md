# Requirements Document

## Introduction
Align translation contexts and type/scheme reification with `papers/these-finale-english.txt`
(using `papers/xmlf.txt` as source of truth) by removing gen-ancestor fallback for Q(n),
making gen-node translation explicit (Q(g) + scheme root), and adding a binding-tree
invariant that rejects fallback-dependent graphs. Out of scope: changing presolution
algorithms or witness normalization beyond binder placement/alignment.

## Requirements

### Requirement 1
**User Story:** As a translator, I want Q(n) and computation contexts to be derived only
from direct binders, so translation is thesis-exact.

#### Acceptance Criteria
1. WHEN computing ordered binders for a type node THEN THE SYSTEM SHALL use only direct
   flex children of that node and SHALL NOT consult any gen-ancestor fallback.
2. WHEN a target node is not reachable via direct binder/bound paths THEN THE SYSTEM
   SHALL return `Nothing` or a structured instantiation error (no fallback path).

### Requirement 2
**User Story:** As a translator, I want gen nodes translated explicitly, so scheme
quantifiers follow Q(g) from the thesis.

#### Acceptance Criteria
1. WHEN translating a gen node g THEN THE SYSTEM SHALL build quantifiers from Q(g)
   (direct flex children of g) ordered by ≺ and with bounds from T(n).
2. WHEN generalizing/reifying a scheme rooted at g THEN THE SYSTEM SHALL use Q(g)
   (direct children) and SHALL NOT use interior-of traversal as a substitute.

### Requirement 3
**User Story:** As a maintainer, I want invariant checks that reject fallback-dependent
graphs, so mis-modeled binders are caught early.

#### Acceptance Criteria
1. WHEN validating a constraint for elaboration THEN THE SYSTEM SHALL detect graphs that
   would require gen-ancestor fallback to recover type-node binders.
2. IF the invariant is violated THEN THE SYSTEM SHALL report a structured error that
   identifies the offending binder node(s) and related gen node(s).

### Requirement 4
**User Story:** As a reviewer, I want tests that lock in thesis-exact behavior.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL pass updated context tests that
   bind Q(n) directly to TyForall nodes.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a positive test for gen-node
   translation (Q(g)) and a negative test that exercises the new invariant check.
