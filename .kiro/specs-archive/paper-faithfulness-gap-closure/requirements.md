# Requirements Document

## Introduction
The `.kiro/specs/paper_general_raise_plan` work brought the implementation much closer to `papers/xmlf.txt` by adding an explicit binding tree and making `Raise`/`Weaken` real binding-edge transformations, with exact `I(r)` and Φ support for non-spine `OpRaise`.

There are still known “paper-faithfulness” gaps:
1) stale documentation that contradicts the current implementation,
2) remaining non-paper fallbacks (silent failure / “shouldn’t happen” behavior) in binding-edge harmonization,
3) node-kind semantics that conflate **restricted** and **locked** (paper §3.1),
4) ω execution logic (Graft/Merge/Weaken) for χe still lives in phase-specific code rather than a shared ω executor.

The much larger follow-up gap (“retire the legacy `GNodeId`/`cGNodes`/`tnVarLevel` scope model”) is tracked separately as `.kiro/specs/scope-model-retirement`.

This spec closes the remaining gaps above while keeping the code warning-free and test-backed.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want the repo’s paper-alignment documentation to match the actual implementation, so reviewers are not misled about current behavior.

#### Acceptance Criteria
1. WHEN searching the repo for stale claims about Raise recording (e.g. “do not yet record arbitrary interior `Raise(n)` sequences”) THEN THE SYSTEM SHALL return no matches.
2. WHEN reading `paper_general_raise_plan.txt` THEN THE SYSTEM SHALL describe the implemented phases as implemented and SHALL NOT describe already-implemented items as “not possible yet”.
3. WHEN running `cabal build` THEN THE SYSTEM SHALL succeed without new warnings introduced by doc-driven refactors.

### Requirement 2
**User Story:** As a paper-alignment implementer, I want node-kind predicates to match `papers/xmlf.txt`’s definitions (instantiable/restricted/locked), so ω legality and Ω normalization are faithful.

#### Acceptance Criteria
1. WHEN a node’s *own* binding edge is rigid THEN THE SYSTEM SHALL classify it as `Restricted` (not `Locked`).
2. WHEN a node’s own binding edge is flexible AND some *ancestor* binding edge is rigid THEN THE SYSTEM SHALL classify it as `Locked`.
3. WHEN all binding edges on a node’s path to the root are flexible THEN THE SYSTEM SHALL classify it as `Instantiable`.
4. WHEN checking “under a rigidly bound node” for Ω normalization THEN THE SYSTEM SHALL treat descendants of a restricted node as “under rigid”, but SHALL NOT treat the restricted node itself as “under rigid” solely because its own edge is rigid.

### Requirement 3
**User Story:** As a solver developer, I want binding-edge harmonization to be fail-fast and paper-faithful, so we don’t silently produce incorrect Ω traces or inconsistent binding trees.

#### Acceptance Criteria
1. WHEN binding-parent harmonization requires a Raise step on a non-instantiable node THEN THE SYSTEM SHALL return a structured error (not silently stop).
2. WHEN the binding tree does not have a valid LCA for two nodes that must be harmonized THEN THE SYSTEM SHALL return a structured error (not use a fallback LCA).
3. WHEN presolution/solve attempts to perform a paper-disallowed Raise THEN THE SYSTEM SHALL surface a `BindingTreeError` (or equivalent phase error) rather than continuing with an inconsistent state.

### Requirement 4
**User Story:** As a maintainer, I want ω execution for χe to be centralized, so paper semantics are implemented once and shared across phases.

#### Acceptance Criteria
1. WHEN presolution executes base ω ops for χe (Graft/Merge/Weaken) THEN THE SYSTEM SHALL call a shared ω executor module (not ad-hoc phase-local helpers).
2. WHEN running existing Ω/Φ-focused tests (Raise + Weaken + Merge) THEN THE SYSTEM SHALL continue to pass after the refactor.
3. WHEN searching `src/MLF/Presolution.hs` THEN THE SYSTEM SHALL no longer contain local “base ops executor” helpers (replaced by the shared module).
