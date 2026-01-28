# Requirements Document

## Introduction
This spec removes elimination pruning from both normalization and emission so
witnesses obey only `papers/xmlf.txt` conditions (1)–(5). The normalizer should
enforce only those conditions, and presolution should avoid adding any extra
pruning rule based on later eliminations.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want witness normalization to match the
paper’s definition (conditions (1)–(5) only), so the normalizer does not impose
extra, undocumented pruning.

#### Acceptance Criteria
1. WHEN `normalizeInstanceOpsFull` runs, THEN THE SYSTEM SHALL NOT drop
   `OpGraft`/`OpWeaken` solely because a binder is eliminated later by
   `OpMerge`/`OpRaiseMerge`.
2. WHEN `normalizeInstanceOpsFull` runs, THEN THE SYSTEM SHALL continue to
   enforce conditions (1)–(5) from `papers/xmlf.txt` (interior, ≺, Raise,
   RaiseMerge, Weaken-after-descendants).

### Requirement 2
**User Story:** As a maintainer, I want presolution emission to stop pruning
operations based on later eliminations, so witnesses reflect only the paper’s
rules.

#### Acceptance Criteria
1. WHEN presolution integrates base ops with extra ops, THEN THE SYSTEM SHALL
   preserve `OpGraft`/`OpWeaken` even if a later `OpMerge`/`OpRaiseMerge`
   eliminates the same binder.
2. WHEN presolution emits a witness without eliminations, THEN THE SYSTEM SHALL
   preserve the emitted ops exactly (no additional pruning).

### Requirement 3
**User Story:** As a reviewer, I want tests to confirm elimination pruning is
removed entirely, so witnesses reflect only paper conditions (1)–(5).

#### Acceptance Criteria
1. WHEN tests cover normalization, THEN THE SYSTEM SHALL confirm it only enforces
   conditions (1)–(5) and does not drop redundant ops.
2. WHEN tests cover emission/integration, THEN THE SYSTEM SHALL confirm
   `integratePhase2Ops` preserves `OpGraft`/`OpWeaken` even when a later
   `OpMerge`/`OpRaiseMerge` eliminates the same binder.
