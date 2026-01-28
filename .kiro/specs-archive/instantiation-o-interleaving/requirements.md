# Requirements Document

## Introduction
This spec aligns quantifier-introduction ("O" / InstIntro) placement with
`papers/xmlf.txt` by interleaving O steps with Omega operations in the
per-edge witness, instead of recording a suffix-only count (`ewForallIntros`).
The goal is to preserve the ordering of ExpForall relative to other expansion
steps and make Phi translation faithful to the paper's sequencing rules.
Omega normalization for these sequences must continue to enforce only
conditions (1)–(5) from `papers/xmlf.txt` (no elimination pruning).

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want quantifier-introduction steps to be
recorded in the witness order, so ExpForall can be interleaved with other
operations per the paper.

#### Acceptance Criteria
1. WHEN presolution constructs a witness for an expansion that includes
   ExpForall, THEN THE SYSTEM SHALL record a corresponding O step in the same
   relative position as that expansion step.
2. WHEN ExpForall contains a ForallSpec with binder count k, THEN THE SYSTEM
   SHALL emit k O steps for that spec (one per binder).
3. WHEN an expansion is a composition (ExpCompose), THEN THE SYSTEM SHALL
   preserve the composition order when emitting witness steps.
4. WHEN an expansion contains no ExpForall, THEN THE SYSTEM SHALL emit no O
   steps and behave as before.

### Requirement 2
**User Story:** As a consumer of witnesses, I want Phi translation to apply O
steps in sequence with Omega operations, so the resulting xMLF instantiation
reflects the intended order.

#### Acceptance Criteria
1. WHEN Phi translation encounters an O step in a witness sequence, THEN THE
   SYSTEM SHALL emit InstIntro at that position in the instantiation.
2. WHEN a witness contains interleaved O and Omega steps, THEN THE SYSTEM SHALL
   translate them in order without reordering across step types.

### Requirement 3
**User Story:** As a reviewer, I want the new witness representation to remain
compatible with normalization and validation rules, so Omega normalization stays
paper-faithful.

#### Acceptance Criteria
1. WHEN normalization is applied to a witness sequence that includes O steps,
   THEN THE SYSTEM SHALL normalize only Omega operations and preserve the
   relative positions of O steps.
2. WHERE Omega normalization requires interior/ordering validation, THEN THE
   SYSTEM SHALL apply those checks only to Omega operations, not O steps.
3. WHEN a witness contains only Omega operations, THEN THE SYSTEM SHALL keep
   behavior identical to the current normalizeInstanceOpsFull pipeline.
