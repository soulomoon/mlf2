# Requirements Document

## Introduction
`papers/xmlf.txt` assumes graphic constraints are rooted: the binding relation is
a (single-rooted) binding tree, so every pair of nodes has a lowest common
ancestor (LCA). In this repo, constraints can contain multiple disconnected
term-DAG components, which can lead to **no common ancestor** in the binding
relation during unification.

This spec makes binding-edge harmonization paper-faithful by:
1) removing the last silent “no LCA ⇒ no-op” fallback, and
2) maintaining a gen-rooted constraint so LCA is total on pipeline constraints
   (matching the paper’s rooted model).

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want binding-edge harmonization to fail when there is no binding LCA, so the implementation matches the paper’s “Raise-to-LCA” model and never silently skips required Raise steps.

#### Acceptance Criteria
1. WHEN `Binding.bindingLCA` is asked for an LCA that does not exist THEN THE SYSTEM SHALL return a structured `BindingError` (not an `InvalidBindingTree` string sentinel).
2. WHEN `BindingAdjustment.harmonizeBindParentsWithTrace` cannot find an LCA THEN THE SYSTEM SHALL return `Left <error>` (not `Right (c, [])`).
3. WHEN `BindingAdjustment.harmonizeBindParents` cannot find an LCA THEN THE SYSTEM SHALL raise an explicit error (not return the input constraint unchanged).

### Requirement 2
**User Story:** As a maintainer, I want tests to lock in the new behavior, so future refactors do not reintroduce silent “no LCA” fallbacks.

#### Acceptance Criteria
1. WHEN harmonizing two nodes from disconnected binding trees in a unit test THEN THE SYSTEM SHALL produce the “no LCA” error.

### Requirement 3
**User Story:** As a maintainer, I want constraints to be rooted (paper-faithful), so binding LCA exists for any two nodes used by the pipeline.

#### Acceptance Criteria
1. WHEN a constraint has more than one term-DAG root THEN THE SYSTEM SHALL record all term-DAG roots in the root gen node’s `gnSchemes`.
2. WHEN creating or maintaining the root gen node THEN THE SYSTEM SHALL bind each term-DAG root under the gen root with `BindFlex` (unless it already has a binding parent).
3. WHEN a constraint already has a root gen node THEN THE SYSTEM SHALL attach any newly-disconnected term-DAG roots as additional schemes (and bind them under the gen root if needed).

### Requirement 4
**User Story:** As a maintainer, I want all of this to be non-regressing, so paper-faithfulness work does not break the rest of the solver.

#### Acceptance Criteria
1. WHEN running the full test suite THEN THE SYSTEM SHALL pass.
