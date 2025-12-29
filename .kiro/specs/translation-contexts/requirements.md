# Requirements Document

## Introduction
Chapter 15.3 introduces translation contexts and computation contexts for
translating gMLF to xMLF. The current implementation has partial context
handling but does not model the full context computation described in the
thesis. This spec aligns context computation and usage with the paper.

## In Scope
- Add or align data structures for translation and computation contexts.
- Implement context computation used by Phi translation for non-spine cases.
- Add tests that check context computation on representative DAGs.

## Out of Scope
- Changes to witness generation beyond context placement.
- Alternative evaluation strategies or reduction semantics.
- Rewriting the presolution pipeline.

## Requirements

### Requirement 1
**User Story:** As a translator, I want context computation to match the thesis
so instantiations are placed correctly.

#### Acceptance Criteria
1. WHEN computing a translation context for a target binder THEN THE SYSTEM
   SHALL return the context steps defined in Chapter 15.3 (under/inside).
2. WHEN a binder is under another binder's bound THEN THE SYSTEM SHALL include
   a computation-context step that goes inside the bound before instantiating.

### Requirement 2
**User Story:** As a maintainer, I want Phi translation to use the computed
contexts consistently.

#### Acceptance Criteria
1. WHEN translating non-spine Raise THEN THE SYSTEM SHALL apply the computed
   context before inserting binders.
2. WHEN translation contexts are missing or invalid THEN THE SYSTEM SHALL
   report a structured error instead of silently falling back.

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in context behavior.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include a regression test
   for a DAG that requires an inside-bound context.
2. WHEN running the test suite THEN THE SYSTEM SHALL include a regression test
   for a DAG that only requires an under-quantifier context.
