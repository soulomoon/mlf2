# Requirements Document

## Introduction
Several repository docs and Kiro specs still mention “expected-failure” paper-alignment baselines in `test/ElaborationSpec.hs`. Those baselines now typecheck, and the surrounding docs/plans should be updated so the repo’s paper-alignment status is accurate and non-confusing.

Scope:
- rename the paper-alignment “expected failure” test labels that now pass
- update Kiro spec notes/requirements that still claim expected-failure baselines
- update `merge_raise_merge_plan.txt` to reflect implemented Phase 8 work

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want test descriptions to reflect reality, so passing “paper alignment baselines” are not labeled as “expected failures”.

#### Acceptance Criteria
1. WHEN searching `test/ElaborationSpec.hs` THEN THE SYSTEM SHALL not contain the suite title `Paper alignment baselines (expected failures)`.
2. WHEN searching `test/ElaborationSpec.hs` THEN THE SYSTEM SHALL not label the κσ annotation baselines as “(expected failure)”.

### Requirement 2
**User Story:** As a spec maintainer, I want Kiro spec docs to reflect the current test status, so readers are not misled by outdated “expected failure” notes.

#### Acceptance Criteria
1. WHEN searching `.kiro/specs` (excluding `.kiro/specs/paper-alignment-doc-sync`) THEN THE SYSTEM SHALL not mention “intentional failing baselines” or “expected-failure baselines” for `cabal test`.
2. WHEN reading `.kiro/specs/scope-model-retirement/requirements.md` THEN THE SYSTEM SHALL not refer to a “non-expected-failure test suite subset”.

### Requirement 3
**User Story:** As a paper-alignment maintainer, I want the Raise/Merge plan doc to reflect what is implemented, so it can be used as an accurate status document.

#### Acceptance Criteria
1. WHEN searching `merge_raise_merge_plan.txt` THEN THE SYSTEM SHALL not describe Phase 8 as “Remaining gaps (TODO)” if the work is implemented.
2. WHEN reading `merge_raise_merge_plan.txt` THEN THE SYSTEM SHALL clearly separate “implemented” vs “future” items for Raise/≺/Φ.
