# Requirements Document

## Introduction
Align witness normalization with `papers/xmlf.txt` condition (5): “Weaken(n) appears after all the other operations on a node below n.” The paper’s examples and translation rules indicate “below n” means strict binding-tree descendants, not n itself. This spec codifies that interpretation and updates tests/comments accordingly.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want `normalizeInstanceOpsFull` to treat “below n” as strict descendants, so Ω normalization matches `papers/xmlf.txt` without over-reordering Weaken.

#### Acceptance Criteria
1. WHEN normalization sees `Weaken(n)` and any op targeting a strict descendant of `n`, THEN THE SYSTEM SHALL place `Weaken(n)` after all operations on those descendants in the normalized sequence.
2. WHEN normalization sees `Weaken(n)` and only ops targeting `n` itself (no descendant ops), THEN THE SYSTEM SHALL preserve the relative order between `Weaken(n)` and same-binder ops (no forced move).
3. WHEN normalization sees `Weaken(n)` alongside unrelated ops outside `n`’s descendant set, THEN THE SYSTEM SHALL preserve the relative order of those unrelated ops.

### Requirement 2
**User Story:** As a reviewer, I want tests and comments to reflect the paper’s strict-descendant interpretation, so regressions are caught and the implementation intent is clear.

#### Acceptance Criteria
1. WHERE tests assert Weaken ordering, THEN THE SYSTEM SHALL test strict-descendant behavior (not “same-binder” reordering).
2. WHERE normalization code documents condition (5), THEN THE SYSTEM SHALL cite `papers/xmlf.txt` and state that “below n” means strict descendants.
