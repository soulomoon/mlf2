# Requirements Document

## Introduction
This spec captures a high-level backlog of thesis alignment work required to
make the implementation track `papers/these-finale-english.txt` in structure
and behavior, while treating `papers/xmlf.txt` as the source of truth when
there is a conflict.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want a high-level inventory of thesis
coverage so I can see which sections are implemented, partial, or missing.

#### Acceptance Criteria
1. WHEN the backlog is authored THEN THE SYSTEM SHALL list each thesis part or
   chapter with a status (implemented, partial, missing) and primary code or
   spec references.
2. WHEN a chapter is partial or missing THEN THE SYSTEM SHALL list follow-on
   work items and name a target spec directory under `.kiro/specs/`.
3. WHEN a thesis detail conflicts with `papers/xmlf.txt` THEN THE SYSTEM SHALL
   record the conflict and the precedence decision.

### Requirement 2
**User Story:** As an implementer, I want prioritized backlog items so I can
schedule work.

#### Acceptance Criteria
1. WHEN backlog items are listed THEN THE SYSTEM SHALL assign a priority label
   (P0, P1, P2) and an effort label (S, M, L).
2. WHEN an item maps to an existing spec THEN THE SYSTEM SHALL link to that
   spec path.
3. WHEN an item requires a new spec THEN THE SYSTEM SHALL propose a spec name.

### Requirement 3
**User Story:** As a reviewer, I want traceable tasks to keep the backlog
current.

#### Acceptance Criteria
1. WHEN tasks are written THEN THE SYSTEM SHALL include a verification step per
   task.
2. WHEN the backlog is updated THEN THE SYSTEM SHALL keep the inventory in
   `design.md` as the single source of truth.
