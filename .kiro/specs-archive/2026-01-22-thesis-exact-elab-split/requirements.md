# Requirements Document

## Introduction

Refactor the elaboration pipeline to be thesis-faithful by moving planning/decision logic (generalization plan, scheme-root policy, binder selection, ordering dependencies, and reify planning) into presolution, leaving `MLF.Elab` as a thin application layer that consumes precomputed plans and witnesses. Behavior must remain unchanged and the test suite must pass.

## Requirements

### Requirement 1 — Presolution builds explicit plans

User Story: As a maintainer, I want presolution to compute explicit generalization and reification plans so elaboration does not make solver-like decisions.

#### Acceptance Criteria

1. WHEN generalization/reification planning is needed, THEN THE SYSTEM SHALL provide explicit plan records (e.g., `GeneralizePlan`, `ReifyPlan`) in a presolution module.
2. WHEN Elab needs binder selection, scheme-root policy, ordering dependencies, or alias policy, THEN THE SYSTEM SHALL obtain them from presolution plans instead of computing them internally.

### Requirement 2 — Elab is a pure plan consumer

User Story: As a maintainer, I want `MLF.Elab` to apply plans and witnesses without introducing new decisions so the pipeline matches the thesis.

#### Acceptance Criteria

1. WHEN elaboration applies instantiations, witnesses, or reification, THEN THE SYSTEM SHALL use only the provided plan records and presolution artifacts (no new policy decisions inside Elab).
2. WHEN Elab currently performs planning logic, THEN THE SYSTEM SHALL move that logic into presolution or a dedicated planning module.

### Requirement 3 — Orchestrator wiring is updated

User Story: As a maintainer, I want the pipeline to wire presolution plans into elaboration so the data flow is explicit.

#### Acceptance Criteria

1. WHEN the elaboration pipeline runs, THEN THE SYSTEM SHALL construct plans in presolution and pass them into Elab apply functions.
2. WHEN wiring changes, THEN THE SYSTEM SHALL preserve existing function behavior and outputs.

### Requirement 4 — Behavior and tests remain stable

User Story: As a reviewer, I want confidence that the refactor does not change semantics.

#### Acceptance Criteria

1. WHEN the refactor is complete, THEN THE SYSTEM SHALL pass `cabal test`.

