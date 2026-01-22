# Requirements Document

## Introduction

Push the pipeline to ~95–100% thesis‑faithful alignment by removing remaining planning/policy logic from `MLF.Elab.Generalize`, decoupling `ElabError` from the Elab layer, and ensuring presolution owns all decision‑making. Elab should be a pure plan consumer. Behavior must remain unchanged and tests must pass.

## Requirements

### Requirement 1 — Elab contains no policy logic

User Story: As a maintainer, I want `MLF.Elab` to only apply precomputed plans so it matches the thesis pipeline boundary.

#### Acceptance Criteria

1. WHEN a generalization/reification decision is made (binder selection, alias policy, scheme‑free‑vars validation, name assignment/renaming policy), THEN THE SYSTEM SHALL implement it in presolution planning modules, not in `MLF.Elab.Generalize`.
2. WHEN `MLF.Elab.Generalize` is used, THEN THE SYSTEM SHALL only apply plan outputs and perform structural rewriting (no new decision logic).

### Requirement 2 — ElabError is neutral

User Story: As a maintainer, I want presolution planning to avoid importing Elab modules by moving `ElabError` to a neutral module.

#### Acceptance Criteria

1. WHEN a presolution plan module needs `ElabError`, THEN THE SYSTEM SHALL import it from a neutral module (not `MLF.Elab.Types`).
2. WHEN `MLF.Elab.Types` needs `ElabError`, THEN THE SYSTEM SHALL re‑export or import it from the neutral module so existing call sites keep working.

### Requirement 3 — Build system and imports are consistent

User Story: As a maintainer, I want module lists and imports to match the new structure.

#### Acceptance Criteria

1. WHEN new neutral or presolution plan modules are added, THEN THE SYSTEM SHALL update `mlf2.cabal` and all imports accordingly.

### Requirement 4 — Behavior is preserved

User Story: As a reviewer, I want confidence the refactor does not change semantics.

#### Acceptance Criteria

1. WHEN the refactor is complete, THEN THE SYSTEM SHALL pass `cabal test`.

