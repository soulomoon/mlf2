# Requirements Document

## Introduction

Finish the last boundary cleanups for a thesis-faithful pipeline by (1) moving Elab type definitions and reify helpers into a neutral namespace so presolution planning has no `MLF.Elab.*` dependency, and (2) lifting the remaining scheme-type fallback decision into plan output. Behavior must remain unchanged and tests must pass.

## Requirements

### Requirement 1 — Neutral Elab types

User Story: As a maintainer, I want presolution planning to use xMLF types from a neutral module so it does not depend on Elab.

#### Acceptance Criteria

1. WHEN presolution plan modules need `ElabType` or `ElabScheme`, THEN THE SYSTEM SHALL import them from a neutral module (e.g., `MLF.Types.Elab`).
2. WHEN existing Elab code uses `MLF.Elab.Types`, THEN THE SYSTEM SHALL continue to work via re-exports (no call-site churn required).

### Requirement 2 — Neutral reify helpers

User Story: As a maintainer, I want reify and type-ops helpers to live in a neutral namespace so presolution planning does not depend on Elab helpers.

#### Acceptance Criteria

1. WHEN presolution plan modules need reify/type-ops helpers, THEN THE SYSTEM SHALL import them from neutral modules (e.g., `MLF.Reify.Core`, `MLF.Reify.TypeOps`).
2. WHEN existing Elab modules use `MLF.Elab.Reify` or `MLF.Elab.TypeOps`, THEN THE SYSTEM SHALL continue to work via re-exports.

### Requirement 3 — Scheme fallback decision is plan-driven

User Story: As a maintainer, I want the scheme-type fallback decision computed in presolution so Elab only applies plans.

#### Acceptance Criteria

1. WHEN a scheme-type fallback decision is needed, THEN THE SYSTEM SHALL compute it in presolution plan output (e.g., `SchemeTypeChoice`) rather than inside `MLF.Elab.Generalize`.
2. WHEN Elab applies generalization, THEN THE SYSTEM SHALL only execute the plan-selected branch without new decision logic.

### Requirement 4 — Build and tests pass

User Story: As a reviewer, I want confidence that refactors are behavior-preserving.

#### Acceptance Criteria

1. WHEN the refactor is complete, THEN THE SYSTEM SHALL pass `cabal test`.

