# Requirements Document

## Introduction
This spec aligns application translation with `papers/xmlf.txt` Figure 7. The
paper states that both the function and the argument positions of an
application are instantiated according to their own instantiation edges. The
current gap is that application elaboration is not guaranteed to instantiate
both sides, or is not verified by tests.

## In Scope
- Ensure application constraint generation emits two instantiation edges
  (function -> arrow, argument -> domain) and records both edge ids in `AApp`.
- Ensure elaboration applies instantiation to both subterms using their
  respective witnesses.
- Add regression tests that lock in dual instantiation behavior.

## Out of Scope
- Changes to witness normalization, Omega/Phi semantics, or expansion logic.
- Changes to generalization, let, lambda, or annotation translation rules.
- Phase 7 xMLF typechecking or reduction semantics.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want application constraint generation to
emit instantiation edges for both the function and argument, so presolution
carries witnesses for each side as in Figure 7.

#### Acceptance Criteria
1. WHEN translating an application `e1 e2`, THEN THE SYSTEM SHALL allocate a
   fresh arrow type `(d -> r)` and emit an instantiation edge `e_fun` from the
   function node to that arrow.
2. WHEN translating an application `e1 e2`, THEN THE SYSTEM SHALL emit an
   instantiation edge `e_arg` from the argument node to the domain `d`.
3. WHEN building the annotated AST, THEN THE SYSTEM SHALL record both edge ids
   in `AApp` and they SHALL be distinct.

### Requirement 2
**User Story:** As an elaboration implementer, I want application translation to
instantiate both subterms with their respective witnesses, so the resulting
xMLF term matches Figure 7's rule.

#### Acceptance Criteria
1. WHEN elaborating an application with a witness for the function edge, THEN
   THE SYSTEM SHALL wrap the translated function in `ETyInst` unless the
   witness is identity.
2. WHEN elaborating an application with a witness for the argument edge, THEN
   THE SYSTEM SHALL wrap the translated argument in `ETyInst` unless the
   witness is identity.
3. WHEN both edges have non-identity witnesses, THEN THE SYSTEM SHALL produce
   `EApp (ETyInst f inst_f) (ETyInst a inst_a)` without reordering.

### Requirement 3
**User Story:** As a reviewer, I want tests that lock in dual instantiation for
applications, so regressions are caught.

#### Acceptance Criteria
1. WHEN running constraint-generation tests, THEN THE SYSTEM SHALL include a
   regression test that asserts application emits two instantiation edges and
   records both edge ids.
2. WHEN running elaboration or pipeline tests, THEN THE SYSTEM SHALL include a
   regression test that `let id = \x. x in id id` elaborates to an application
   with instantiation on both sides.
