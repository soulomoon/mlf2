# Requirements Document

## Introduction
The thesis treats gen nodes (G) as a separate sort from type nodes, with a single
root gen node and strict binding constraints. Our current encoding conflates gen
nodes with `TyForall`/`TyRoot` and introduces synthetic roots. This spec restores
thesis-exact structure by separating gen nodes and enforcing the invariants.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want gen nodes represented separately from type
nodes so the constraint matches the thesis structure.

#### Acceptance Criteria
1. WHEN constructing a constraint THEN gen nodes SHALL be stored in a dedicated
   map keyed by `GenNodeId`.
2. WHEN traversing binding edges THEN gen nodes SHALL be distinguished from type
   nodes via `NodeRef`.

### Requirement 2
**User Story:** As a reviewer, I want the constraint to remain thesis-rooted
without synthetic insertion.

#### Acceptance Criteria
1. WHEN generating constraints THEN the system SHALL create a single root gen node.
2. WHEN later phases run THEN they SHALL NOT create synthetic roots.
3. WHEN a constraint violates rootedness THEN the system SHALL return a structured
   error.

### Requirement 3
**User Story:** As an implementer, I want binding invariants enforced to match the
thesis.

#### Acceptance Criteria
1. WHEN a gen node has a binding parent THEN the parent SHALL be a gen node.
2. WHEN a type node is bound THEN its binding ancestor chain SHALL reach a gen node.
3. WHEN a gen node is targeted by a raise/merge/instantiate operation THEN the
   system SHALL reject the operation.

### Requirement 4
**User Story:** As an elaborator, I want named nodes and contexts derived from gen
nodes only.

#### Acceptance Criteria
1. WHEN enumerating named nodes THEN only type nodes bound on gen nodes SHALL be
   returned.
2. WHEN computing translation contexts THEN ordered binders SHALL be filtered to
   named nodes bound on gen nodes.

## Out of Scope
- Algorithmic changes to presolution beyond data-model alignment.
- Performance tuning unrelated to correctness.
