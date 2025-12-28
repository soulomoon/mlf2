# Requirements Document

## Introduction
The repo currently performs a lightweight normalization of instantiation
witnesses (Ω). `papers/xmlf.txt` defines a **normalized witness** by explicit
conditions (1)–(5) and references Yakobowski'08 for a constructive
normalization algorithm. This spec defines the requirements to implement the
paper-faithful normalization of Ω while keeping the rest of the pipeline
stable.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want Ω normalization to satisfy the paper's
conditions (1)–(5), so that Φ translation is faithful to `papers/xmlf.txt`.

#### Acceptance Criteria
1. WHEN normalization completes THEN THE SYSTEM SHALL output a witness Ω that
   satisfies conditions (1)–(5) from `papers/xmlf.txt` §3.4:
   (1) Graft/Weaken only on nodes in I(r);
   (2) Merge(n, m) only when n,m ∈ I(r) and m ≺ n;
   (3) Raise(n) only when n is bound under r in the binding tree;
   (4) RaiseMerge(n, m) only when n ∈ I(r) and m ∉ I(r);
   (5) Weaken(n) appears after all other ops on nodes below n.
2. WHEN an input witness Ω violates these conditions THEN THE SYSTEM SHALL
   return a structured normalization error (not silently accept).

### Requirement 2
**User Story:** As a maintainer, I want the normalization to be deterministic
and stable, so future refactors do not change witness ordering unexpectedly.

#### Acceptance Criteria
1. WHEN the same inputs are normalized twice THEN THE SYSTEM SHALL produce
   identical output Ω sequences.
2. WHEN the input contains semantically equivalent commutable ops THEN THE
   SYSTEM SHALL apply a deterministic tie-breaker based on ≺ ordering.

### Requirement 3
**User Story:** As a maintainer, I want normalization to be verifiable with
unit and property tests, so the paper conditions are enforced by tests.

#### Acceptance Criteria
1. WHEN normalization is implemented THEN THE SYSTEM SHALL include unit tests
   for each paper condition (1)–(5).
2. WHEN normalization is implemented THEN THE SYSTEM SHALL include at least one
   property test that validates all five conditions on the normalized output.

### Requirement 4
**User Story:** As a pipeline user, I want the presolution to keep working
without API breakage, so normalization remains usable by existing call sites.

#### Acceptance Criteria
1. WHEN normalization API changes are introduced THEN THE SYSTEM SHALL keep a
   compatibility wrapper for existing call sites (or update all call sites in
   the same change set).
2. WHEN normalization requires context (I(r), ≺ keys, binding tree) THEN THE
   SYSTEM SHALL provide that context at the call site from existing data
   (`EdgeTrace`, constraint, union-find).
