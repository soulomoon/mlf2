# Requirements Document

## Introduction
Chapter 8 describes translations between syntactic types/terms and graphic
representations, including bound inlining rules. The current implementation
covers a subset of reification but does not explicitly codify the full
translation and inlining rules. This spec aligns those translations with the
thesis.

## In Scope
- Audit and align syntactic-to-graphic and graphic-to-syntactic translation
  rules (Chapter 8).
- Implement bound inlining rules during reification where required.
- Add tests that validate translation round-trips on representative graphs.

## Out of Scope
- Overhauling constraint solving or presolution logic.
- Changes to surface syntax or parser behavior.
- Optimizations unrelated to translation correctness.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want reification to follow Chapter 8 so
graphic nodes map to the intended syntactic types.

#### Acceptance Criteria
1. WHEN reifying a graphic type THEN THE SYSTEM SHALL apply the bound inlining
   rules specified in Chapter 8.
2. WHEN encountering shared graphic structure THEN THE SYSTEM SHALL produce a
   deterministic syntactic type that matches the paper's translation.

### Requirement 2
**User Story:** As a maintainer, I want syntactic-to-graphic translation to be
complete for the xMLF features we support.

#### Acceptance Criteria
1. WHEN internalizing a syntactic type THEN THE SYSTEM SHALL construct a
   graphic representation with correct binding edges.
2. WHEN internalizing bounded quantification THEN THE SYSTEM SHALL preserve
   bounds and binder identities.

### Requirement 3
**User Story:** As a reviewer, I want translation tests that prevent
regressions.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include a round-trip test
   that reifies a graph and compares it to an expected syntactic type.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a test covering
   bound inlining behavior.
