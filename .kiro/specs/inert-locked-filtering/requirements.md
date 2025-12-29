# Requirements Document

## Introduction
The thesis (section 15.2.3) requires that propagation witnesses never
transform inert-locked nodes. These nodes cannot be translated into xMLF
operations, so the implementation must either eliminate them or reject
presolutions containing them. This spec defines the detection and filtering
(or weakening) needed to align translation with the thesis.

## In Scope
- Define inert and inert-locked nodes per `papers/these-finale-english.txt`.
- Remove or rewrite operations that transform inert-locked nodes in
  propagation witnesses.
- Introduce a presolution rewrite that weakens inert-locked nodes to a
  translatable form when possible.
- Tests that ensure no propagation witness transforms an inert-locked node.

## Out of Scope
- xMLF typechecking or reduction semantics (Phase 7).
- Changes to the core constraint generator beyond exposing required metadata.
- eMLF/iMLF translation (handled in separate specs).

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want inert-locked nodes to be identified so
translation can avoid unsupported operations.

#### Acceptance Criteria
1. WHEN computing the inert-locked set THEN THE SYSTEM SHALL classify a node
   `n` as inert-locked if it satisfies the thesis definition (inert, flexibly
   bound, and its binding path contains a rigid edge) from section 15.2.2.
2. WHEN the inert-locked set is computed THEN THE SYSTEM SHALL expose it to
   witness normalization and translation stages.

### Requirement 2
**User Story:** As a translator, I want propagation witnesses to avoid
transforming inert-locked nodes so xMLF translation is defined.

#### Acceptance Criteria
1. WHEN building a propagation witness THEN THE SYSTEM SHALL NOT emit
   operations that transform inert-locked nodes.
2. WHEN a presolution contains inert-locked nodes THEN THE SYSTEM SHALL
   attempt to weaken them to a translatable form (per Lemma 15.2.4) or return
   a structured error if it cannot.

### Requirement 3
**User Story:** As a reviewer, I want tests that detect regressions in
inert-locked filtering.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include a regression test
   where operations on inert-locked nodes are removed or rejected.
2. WHEN normalization succeeds THEN THE SYSTEM SHALL ensure no emitted
   operation targets an inert-locked node (unit or property test).
