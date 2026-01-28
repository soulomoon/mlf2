# Requirements Document

## Introduction
This spec refactors explicit recursive traversals in the elaboration and
frontend layers to use recursion-schemes. The goal is to make structural
operations more uniform, use advanced schemes where they improve clarity,
and preserve existing behavior and paper-aligned semantics.

## In Scope
- Replace safe explicit recursion on Elab and Frontend trees with
  recursion-schemes folds/unfolds.
- Use advanced schemes (para/apo/zygo/histo) where they simplify combined
  computations or require original subtrees.
- Provide effectful folds for traversals that return Maybe/Either/Monad
  results.
- Document the recursion-schemes reference used for implementation guidance.

## Out of Scope
- Algorithmic or semantic changes to elaboration, binding, or constraints.
- Performance work beyond what is needed to keep behavior identical.
- Changes to public API surface beyond helper exposure.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want recursion-schemes used for safe
traversals so structural code is uniform and easier to audit.

#### Acceptance Criteria
1. WHEN replacing explicit recursion in Elab or Frontend traversals, THEN THE
   SYSTEM SHALL preserve existing results and binder behavior.
2. WHEN a traversal requires access to the original subtree, THEN THE SYSTEM
   SHALL use a scheme that provides it (e.g., para, zygo, histo, or apo).
3. WHEN a traversal combines multiple computed properties, THEN THE SYSTEM
   SHALL use a scheme that avoids multiple passes (e.g., zygo).

### Requirement 2
**User Story:** As a maintainer, I want effectful traversals expressed as
recursion-schemes helpers so error handling stays consistent.

#### Acceptance Criteria
1. WHEN a traversal returns Maybe/Either/Monad results, THEN THE SYSTEM SHALL
   use an effectful fold helper (e.g., cataM/cataEither/cataMaybe) rather than
   manual recursion.
2. WHEN new helpers are introduced, THEN THE SYSTEM SHALL keep signatures
   minimal and aligned with existing module boundaries.

### Requirement 3
**User Story:** As a reviewer, I want a clear reference for the recursion-
schemes API used by the refactor.

#### Acceptance Criteria
1. WHEN updating the spec, THEN THE SYSTEM SHALL reference the official
   Data.Functor.Foldable documentation for recursion-schemes 5.2.3.

### Requirement 4
**User Story:** As a maintainer, I want confidence that refactors did not
change outputs.

#### Acceptance Criteria
1. WHEN the refactor is complete, THEN THE SYSTEM SHALL keep the existing test
   suite passing.
