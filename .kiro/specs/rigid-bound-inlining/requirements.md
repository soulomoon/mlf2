# Requirements Document

## Introduction
The paper’s translation inlines rigidly bound variables (R(n)=T(n)). Our
implementation currently leaves rigid TyVar nodes as variables, which breaks
paper-aligned generalization once rigid edges are respected. This spec aligns
reification and non-Forall generalization with the paper by inlining rigid
vars and treating their bounds as reachable.

## In Scope
- Inline rigid TyVar nodes by reifying their bounds (⊥ when no bound exists).
- Extend non-Forall generalization to follow binding paths while excluding
  restricted nodes (those whose own binding edge is rigid).
- Treat rigid-variable bounds as reachable when collecting binders at a
  non-Forall scope.
- Add a regression test that exercises rigid-bound inlining.

## Out of Scope
- Changing presolution or witness generation.
- Reworking the binding-tree model or its ordering rules beyond existing ≺.
- Altering the representation of bounds or eliminated vars.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want rigidly bound variables to inline their
bounds so xMLF translation matches `papers/xmlf.txt`’s R(n)=T(n).

#### Acceptance Criteria
1. WHEN reifying a TyVar with a rigid binding edge, THEN THE SYSTEM SHALL
   reify its bound instead of emitting a type variable.
2. WHEN a rigid TyVar has no stored bound, THEN THE SYSTEM SHALL reify it as
   ⊥ (bottom) to keep types closed.
3. WHEN reifying non-rigid TyVars, THEN THE SYSTEM SHALL continue emitting
   a type variable name.

### Requirement 2
**User Story:** As a pipeline user, I want top-level generalization to respect
rigid edges without producing open types.

#### Acceptance Criteria
1. WHEN generalizing at a non-Forall scope, THEN THE SYSTEM SHALL exclude
   variables whose own binding edge is rigid (restricted nodes), while
   allowing locked descendants of rigid binders.
2. WHEN rigid variables are excluded, THEN THE SYSTEM SHALL still quantify
   variables reachable through their bounds so the resulting scheme is closed.
3. WHEN elaborating `let id = (\x. x) in id id`, THEN THE SYSTEM SHALL still
   report the polymorphic type ∀a. a -> a.

### Requirement 3
**User Story:** As a reviewer, I want regression coverage for rigid-bound
inlining so paper-aligned behavior stays stable.

#### Acceptance Criteria
1. WHEN rigid-bound inlining is implemented, THEN THE SYSTEM SHALL add a
   regression test that constructs a rigid TyVar with a bound and checks
   `generalizeAt` output.
2. WHEN running the test suite, THEN THE SYSTEM SHALL keep existing paper
   alignment baselines passing.
