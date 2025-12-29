# Requirements Document

## Introduction
To remove the remaining generalization fallback, the binding tree must fully
capture all quantifiable variables. This spec defines the work needed to ensure
binding-parent edges are complete for variables introduced by expansions/copies
so Q(n) (and binding-path enumeration for non-Forall scopes) is sufficient.

## In Scope
- Audit and fix binding-parent assignment for variables introduced during:
  - expansion copying (binder metas and copied nodes),
  - forall introductions, and
  - presolution rewrite/canonicalization steps.
- Add regression coverage showing that free variables in a generalized type are
  reachable via binding-parent paths to the scope root (no fallback needed).
- Document the intended invariants for binding-edge completeness.

## Out of Scope
- Removing the fallback in `generalizeAt` (done in follow-up once invariants hold).
- Changing expansion decision logic or solver algorithms beyond binding edges.
- Reworking the binding-tree data model.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want binding edges to account for every
quantifiable variable so Q(n) matches the paper.

#### Acceptance Criteria
1. WHEN a new `TyVar` is introduced during expansion or copy, THEN THE SYSTEM
   SHALL attach a binding-parent edge that places it under the correct scope
   root or binder (flexibly bound).
2. WHEN a `TyForall` is introduced, THEN THE SYSTEM SHALL ensure its quantified
   variables are flexibly bound directly to that `TyForall` (Q(n)).
3. WHEN presolution rewrites/canonicalizes the graph, THEN THE SYSTEM SHALL
   preserve binding-parent relationships for all live nodes.

### Requirement 2
**User Story:** As a reviewer, I want tests that demonstrate binding-edge
coverage so we can safely remove fallback.

#### Acceptance Criteria
1. WHEN generalizing a top-level expression, THEN THE SYSTEM SHALL have all
   free variables in the type reachable via binding-parent paths to the
   constraint root.
2. WHEN generalizing a non-Forall scope, THEN THE SYSTEM SHALL enumerate the
   same binders via binding-parent paths as appear free in the reified type.
3. WHEN running the test suite, THEN THE SYSTEM SHALL include at least one
   regression that would fail if binding edges were missing (bounded aliasing or
   let-polymorphism scenarios).

### Requirement 3
**User Story:** As a maintainer, I want a clear checklist before removing
fallback.

#### Acceptance Criteria
1. WHEN this spec’s tasks are complete, THEN THE SYSTEM SHALL document the
   binding-edge invariants and identify the exact follow-up change to remove the
   fallback.
