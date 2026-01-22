# Requirements Document

## Introduction

Complete the thesis-faithful boundary shift by physically moving generalization planning modules out of `MLF.Elab.Generalize.*` into a presolution planning namespace. The goal is to ensure presolution owns all planning/decision logic, while Elab only applies explicit plans. Behavior must not change, and tests must pass.

## Requirements

### Requirement 1 — Planning modules live under presolution

User Story: As a maintainer, I want generalization planning modules to live under `MLF.Constraint.Presolution.Plan.*` so the pipeline boundary matches the thesis.

#### Acceptance Criteria

1. WHEN a planning helper is needed (binder selection, scheme roots, ordering deps, reify plan), THEN THE SYSTEM SHALL define it under `MLF.Constraint.Presolution.Plan.*` rather than `MLF.Elab.Generalize.*`.
2. WHEN planning modules are moved, THEN THE SYSTEM SHALL update all imports to use the new presolution namespace (no re-export shims under `MLF.Elab.Generalize.*`).

### Requirement 2 — Elab is a plan consumer

User Story: As a maintainer, I want Elab to depend only on plan records and apply them structurally.

#### Acceptance Criteria

1. WHEN Elab uses planning types or helpers, THEN THE SYSTEM SHALL import them from `MLF.Constraint.Presolution.Plan.*`.
2. WHEN `MLF.Constraint.Presolution.Plan` builds plans, THEN THE SYSTEM SHALL no longer import any `MLF.Elab.Generalize.*` modules.

### Requirement 3 — Build system reflects new module layout

User Story: As a maintainer, I want module lists to match the new file layout.

#### Acceptance Criteria

1. WHEN modules move, THEN THE SYSTEM SHALL update `mlf2.cabal` `other-modules` to remove `MLF.Elab.Generalize.*` planning modules and add the new presolution plan modules.

### Requirement 4 — Behavior is preserved

User Story: As a reviewer, I want confidence that the refactor is behavior-preserving.

#### Acceptance Criteria

1. WHEN the refactor is complete, THEN THE SYSTEM SHALL pass `cabal test`.

