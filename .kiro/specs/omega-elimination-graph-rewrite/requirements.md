# Requirements Document

## Introduction
The paper's omega operations (Graft/Weaken/Merge) produce a graph where eliminated
binders are removed from the binding tree and inlined by their bounds. Our
implementation currently records eliminated binders in `cEliminatedVars` and
relies on elaboration-time filtering. This spec replaces that marker with a
paper-aligned graph rewrite step so elimination is structural.

## In Scope
- Rewrite the constraint graph after omega execution to inline eliminated binders.
- Remove eliminated binders from the binding tree (Q(n) enumeration).
- Eliminate reliance on `cEliminatedVars` in reification and generalization.
- Preserve paper-aligned behavior for bounded aliasing and related baselines.

## Out of Scope
- Changing witness generation or omega normalization logic.
- Redesigning instantiation semantics or solver algorithms.
- Implementing xMLF phase-7 semantics (covered by another spec).

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want omega elimination to be represented as a
graph rewrite so translation uses Q/R/T directly.

#### Acceptance Criteria
1. WHEN an omega operation eliminates a binder `v`, THEN THE SYSTEM SHALL remove
   `v` from the binding tree so it is not enumerated by Q(n).
2. WHEN an eliminated binder `v` has a bound `b`, THEN THE SYSTEM SHALL replace
   references to `v` in the term-DAG with `b` (canonicalized).
3. WHEN an eliminated binder `v` has no bound (bottom), THEN THE SYSTEM SHALL replace
   references to `v` with an explicit bottom representation in the graph.
4. WHEN the rewrite finishes, THEN `checkBindingTree` SHALL succeed on the
   rewritten constraint.

### Requirement 2
**User Story:** As a pipeline user, I want elaboration to be independent of
elimination markers.

#### Acceptance Criteria
1. WHEN reifying types or generalizing schemes, THEN THE SYSTEM SHALL NOT
   consult `cEliminatedVars`.
2. WHEN elaborating the paper baselines (`id id`, bounded aliasing,
   `\y. let id = (\x. x) in id y`), THEN THE SYSTEM SHALL still produce the
   paper-aligned schemes.

### Requirement 3
**User Story:** As a reviewer, I want tests that guard the elimination rewrite.

#### Acceptance Criteria
1. WHEN running the test suite, THEN the elimination rewrite SHALL have
   regression coverage for both bounded and unbounded binder elimination.
2. WHEN rewriting eliminated binders, THEN a unit or property test SHALL
   confirm that no eliminated binder appears in the final Q(n) enumeration.
