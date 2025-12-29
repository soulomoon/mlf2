# Requirements Document

## Introduction
`generalizeAt` must stay paper-faithful (Q(n) from the binding tree) while
keeping top-level types closed under the constraint root (`TyRoot`). With
binding-edge coverage in place, generalization now relies solely on binding
paths (no free-variable fallback).

## In Scope
- Use a synthetic `TyRoot` binding scope for top-level generalization.
- Enumerate binders via binding-tree paths:
  - `TyForall` scopes use Q(n) (flex children).
  - Non-Forall scopes use binding-path reachability to the scope root.
- Remove the free-variable fallback and rely on binding-edge enumeration.
- Preserve explicit `TyForall` quantifiers that belong to the target type.

## Out of Scope
- Reworking presolution or solver behavior beyond generalization inputs.
- Replacing the binding-tree model or reifying explicit source-level binder names.
- Renaming or removing the synthetic root node once the binding tree is fully paper-faithful.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want binder enumeration to follow the binding
edges (Q(n)) wherever possible so paper ordering and scoping stay intact.

#### Acceptance Criteria
1. WHEN the scope root is a `TyForall`, THEN THE SYSTEM SHALL quantify exactly
   the flexibly bound children reachable from the order root (excluding
   eliminated vars).
2. WHEN the scope root is not a `TyForall`, THEN THE SYSTEM SHALL select
   variables whose binding-parent path reaches the scope root and deduplicate
   binders by canonical representative.
3. WHEN the target type root is a `TyForall` and the scope root differs, THEN
   THE SYSTEM SHALL preserve the explicit quantifier in the reified type.

### Requirement 2
**User Story:** As a pipeline user, I want top-level generalization to be closed
and predictable without hiding explicit quantifiers.

#### Acceptance Criteria
1. WHEN elaborating a solved constraint, THEN THE SYSTEM SHALL call
   `generalizeAt` with the constraint root (`TyRoot`) as the scope root (falling
   back to the expression root only when the root is absent).
2. WHEN the scope root is non-Forall, THEN THE SYSTEM SHALL not inject
   free-variable fallback binders and SHALL rely on binding edges to close the
   scheme.
3. WHEN a top-level term returns a polymorphic let-bound value, THEN THE SYSTEM
   SHALL still report a polymorphic type.

### Requirement 3
**User Story:** As a reviewer, I want tests and docs that record the fallback
retirement so regressions are visible.

#### Acceptance Criteria
1. WHEN running the test suite, THEN THE SYSTEM SHALL include regression tests
   for generalization on a non-Forall scope and on an explicit `TyForall` target.
2. WHEN fallback is removed, THEN THE SYSTEM SHALL document the rationale and
   scope of the retirement in the design doc.
