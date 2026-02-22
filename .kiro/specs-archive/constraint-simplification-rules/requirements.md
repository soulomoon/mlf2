# Requirements Document

## Introduction
Chapter 12.4 defines three constraint simplification rules (Var-Abs, Var-Let,
ML-Extrude) that reduce constraints while preserving solutions. The current
implementation already achieves the correct simplification behavior, but
implicitly — through constraint generation design and presolution expansion
handling. This spec covers making the rules explicit and thesis-auditable.

## Current State
- **Var-Abs** (§12.4.1, Lemma 12.4.1): Implicit. Lambda parameters never
  create gen nodes; they bind monomorphically at the current scope
  (`ConstraintGen/Translate.hs:146-159`). Degenerate foralls handled during
  expansion (`Presolution/Expansion.hs:130-163`).
- **Var-Let** (§12.4.1, Lemma 12.4.2): Implicit. Let-bound variables use
  expansion nodes; trivial scheme edges cleaned up by
  `dropTrivialSchemeEdges` (`Presolution/Base.hs:671-684`).
- **ML-Extrude** (§12.4.2, Lemma 12.4.3): Intentionally not implemented.
  Only valid for ML constraints; would weaken MLF principal solutions per
  the thesis itself.

## In Scope
- Add thesis-aligned Note references documenting where each rule is realized.
- Add per-rule tests that demonstrate solution preservation.

## Out of Scope
- Extracting rules into standalone rewrite functions (the implicit approach
  is architecturally sound; forcing explicit rewrites would add complexity
  without correctness benefit).
- ML-Extrude implementation (not applicable to MLF).
- Changing presolution behavior.

## Requirements

### Requirement 1: Thesis-auditable documentation
**User Story:** As a reviewer, I want to trace each Chapter 12.4 rule to its
implementation site so I can verify correctness.

#### Acceptance Criteria
1. WHEN reviewing Var-Abs THEN there SHALL be a Note [Constraint
   simplification: Var-Abs (Ch 12.4.1)] at the implementation site explaining
   the correspondence.
2. WHEN reviewing Var-Let THEN there SHALL be a Note [Constraint
   simplification: Var-Let (Ch 12.4.1)] at the implementation site explaining
   the correspondence.
3. WHEN reviewing ML-Extrude THEN there SHALL be a Note documenting why it
   is intentionally omitted for MLF.

### Requirement 2: Per-rule test coverage
**User Story:** As a reviewer, I want tests that lock in each simplification
rule's solution-preservation property.

#### Acceptance Criteria
1. WHEN running the test suite THEN there SHALL be a test demonstrating
   Var-Abs: lambda parameters are monomorphic (no gen node created).
2. WHEN running the test suite THEN there SHALL be a test demonstrating
   Var-Let: trivial let-binding expansion is identity.
