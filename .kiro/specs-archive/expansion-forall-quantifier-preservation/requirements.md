# Requirements Document

## Introduction
When an expansion recipe instantiates binders and then reintroduces quantifiers
(`ExpInstantiate` followed by `ExpForall`), the instantiation witness must not
rigidify the binder metas before the new forall is introduced. Otherwise, Q(n)
becomes empty and explicit quantifiers are lost (e.g., bounded aliasing
baseline). We suppress `OpWeaken` in those instantiation steps while preserving
existing ordering and behavior elsewhere.

## In Scope
- Detect `ExpForall` that appear after `ExpInstantiate` in expansion order.
- Suppress `OpWeaken` emission for those instantiation steps.
- Preserve existing `StepIntro` ordering for `ExpCompose`.
- Keep current behavior when `ExpForall` appears only before instantiation or
  does not appear at all.

## Out of Scope
- Changing expansion decision logic (`decideMinimalExpansion`).
- Rewriting binding-tree invariants or witness normalization rules.
- Altering Phi/Sigma translation beyond the witness step stream.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want quantifiers to survive instantiate-then-
forall expansions so paper Q(n) remains non-empty.

#### Acceptance Criteria
1. WHEN an expansion contains `ExpInstantiate` with a later `ExpForall`, THEN
   THE SYSTEM SHALL omit `OpWeaken` for those instantiation steps.
2. WHEN an expansion has `ExpForall` only before instantiation, THEN THE SYSTEM
   SHALL emit `OpWeaken` as before.
3. WHEN elaborating the bounded aliasing baseline, THEN THE SYSTEM SHALL report
   `∀a. a -> a -> a`.

### Requirement 2
**User Story:** As a reviewer, I want witness ordering to remain stable.

#### Acceptance Criteria
1. WHEN running the presolution witness tests, THEN THE SYSTEM SHALL preserve
   `ExpCompose` ordering with `StepIntro`.
2. WHEN running the full test suite, THEN THE SYSTEM SHALL not regress on
   instantiation witness ordering or normalization checks.
