# Requirements Document

## Introduction
Phase 7 (xMLF typechecking and reduction semantics) is not implemented. This
spec implements the semantics described in `papers/xmlf.txt` Figure 4
(typechecking) and Figure 5 (small-step reduction). The goal is to validate
Phase 6 elaboration output and provide a reference evaluator for xMLF terms.

## In Scope
- A typechecker for `ElabTerm` and `Instantiation` using the xMLF rules.
- A small-step reducer for `ElabTerm` following Figure 5 evaluation contexts.
- Pipeline/API hooks to run Phase 7 after elaboration.
- Tests for both typechecking and reduction.

## Out of Scope
- Changes to Phase 1-6 inference or witness construction.
- Optimization, normalization-by-evaluation, or interpreter performance work.
- Alternative evaluation strategies beyond the one specified in the design.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want an xMLF typechecker that mirrors the
paper, so elaborated terms can be validated against Figure 4.

#### Acceptance Criteria
1. WHEN typechecking an elaborated term, THEN THE SYSTEM SHALL compute its
   type using the rules in `papers/xmlf.txt` Figure 4.
2. WHEN checking an instantiation application (`ETyInst`), THEN THE SYSTEM
   SHALL validate the instantiation against the input type and return a
   structured error if the instantiation is invalid.
3. WHEN typechecking fails, THEN THE SYSTEM SHALL return a descriptive error
   that identifies the failing construct (variable lookup, instantiation,
   application, or abstraction).

### Requirement 2
**User Story:** As a maintainer, I want a small-step reducer for xMLF terms, so
we can execute elaborated programs per the paper's semantics.

#### Acceptance Criteria
1. WHEN reducing a term, THEN THE SYSTEM SHALL implement the small-step rules
   in `papers/xmlf.txt` Figure 5, including beta reduction and instantiation
   steps.
2. WHEN a term is a value under the chosen evaluation strategy, THEN THE SYSTEM
   SHALL report that no reduction steps are available.
3. WHEN a reduction step applies, THEN THE SYSTEM SHALL return the next term
   without changing unrelated subterms (context-respecting reduction).

### Requirement 3
**User Story:** As a pipeline user, I want a Phase 7 entry point, so I can
validate and optionally reduce elaborated terms after Phase 6.

#### Acceptance Criteria
1. WHEN running a Phase 7 pipeline entry point, THEN THE SYSTEM SHALL run
   Phase 1-6, typecheck the elaborated term, and return the type alongside the
   term when successful.
2. WHEN Phase 7 typechecking fails, THEN THE SYSTEM SHALL return a failure
   without crashing and without mutating earlier-phase outputs.
3. WHEN a user requests evaluation, THEN THE SYSTEM SHALL provide a reducer
   that can step or normalize the elaborated term.

### Requirement 4
**User Story:** As a reviewer, I want tests that cover Phase 7, so behavior is
locked in and regressions are detected early.

#### Acceptance Criteria
1. WHEN running the test suite, THEN THE SYSTEM SHALL include unit tests for
   typechecking the core xMLF term constructors (var, lam, app, let, inst,
   type abs).
2. WHEN running the test suite, THEN THE SYSTEM SHALL include reduction tests
   that cover beta-reduction and instantiation reduction.
3. WHEN running the test suite, THEN THE SYSTEM SHALL include at least one
   property test candidate (or regression) that checks type preservation for a
   small, fixed set of terms.
