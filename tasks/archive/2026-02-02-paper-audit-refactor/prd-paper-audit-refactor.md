# PRD: Paper Audit Refactor (Readability and Maintainability)

## Introduction / Overview

The current Haskell implementation is paper-faithful but concentrated in large modules, especially in presolution, unification, and elaboration. This makes maintenance and paper-faithfulness auditing slow and error-prone. This PRD defines a refactor that improves readability and maintainability while making paper alignment easier to verify against the target paper `papers/these-finale-english.txt`, without changing algorithms or user-visible behavior.

Scope is the full refactor plan (Phases 1 to 4): presolution state access hygiene, unification consolidation, elaboration API cleanup, and module boundary tightening. Module moves and renames are allowed if the public API remains stable via re-exports.

## Goals

- Improve readability of presolution and elaboration code by reducing ad-hoc state access and long parameter lists.
- Make paper-faithfulness auditing faster via structured documentation and explicit paper anchors.
- Consolidate unification logic into a single reusable core with phase-specific policies.
- Keep behavior unchanged; builds remain warning-free and tests pass.

## User Stories

### US-001: Presolution state access consolidation
**Description:** As a maintainer, I want presolution modules to use shared state-access helpers so that invariants are enforced consistently and auditing is easier.

**Acceptance Criteria:**
- [ ] Replace ad-hoc access to presolution state in presolution submodules, except Presolution.Base, Presolution.Ops, and Presolution.StateAccess.
- [ ] Add any missing helpers to MLF.Constraint.Presolution.StateAccess or MLF.Constraint.Presolution.Ops instead of duplicating access patterns.
- [ ] Cabal build all passes with warnings enabled.

### US-002: Edge processing factoring with EdgeCtx
**Description:** As a maintainer, I want edge-processing logic split into small helpers with explicit paper anchors so it is easier to audit and reason about.

**Acceptance Criteria:**
- [ ] Introduce an EdgeCtx record capturing per-edge environment (edge id, canonical function, constraint snapshot, suppressWeaken, trace config) and use it across edge-processing helpers.
- [ ] Split MLF.Constraint.Presolution.EdgeProcessing into focused helpers or modules (node resolution, expansion decisions, witness construction, edge-local unify).
- [ ] Add Note blocks for paper-sensitive decisions (Raise, Merge, Weaken handling and witness emission).
- [ ] All presolution tests pass.

### US-003: Unification core consolidation
**Description:** As a maintainer, I want Normalize, Solve, and Presolution to share a common unification core with explicit policies so behavior is consistent and easier to audit.

**Acceptance Criteria:**
- [ ] Introduce a UnifyStrategy record (occurs-check on or off, TyExp policy, forall arity policy, representative choice) in a shared unification module.
- [ ] Normalize, Solve, and Presolution use the shared unification core with explicit strategies matching current behavior.
- [ ] Cabal test passes without changing existing test expectations.

### US-004: Elaboration API cleanup
**Description:** As a maintainer, I want elaboration entry points to take structured config or env records to reduce parameter sprawl and improve clarity.

**Acceptance Criteria:**
- [ ] Replace long elaborate function parameter lists with ElabConfig and ElabEnv records or equivalent.
- [ ] Move expansionToInst to a legacy or debug module and keep the witness-based path as the primary route.
- [ ] Public API remains stable via re-exports; tests pass.

### US-005: Type and graph module boundary tightening
**Description:** As a maintainer, I want smaller, purpose-focused type and graph modules so that auditing and navigation are easier.

**Acceptance Criteria:**
- [ ] Split MLF.Constraint.Types.Graph into smaller submodules (node and edge definitions, binding-tree helpers, accessors) and re-export through MLF.Constraint.Types.
- [ ] Update imports and Cabal module lists to compile cleanly.
- [ ] Cabal build all passes without warnings.

### US-006: Paper-faithfulness documentation index
**Description:** As an auditor, I want a structured paper-to-code map and phase notes so I can verify alignment quickly.

**Acceptance Criteria:**
- [ ] Add docs/paper-map.md mapping `papers/these-finale-english.txt` sections and figures to modules and functions with explicit deviations.
- [ ] Add docs/phase-notes.md with a short section per phase listing invariants and related tests.
- [ ] Update implementation_notes.md where changes affect documented behavior or module layout.

## Functional Requirements

- FR-1: Presolution submodules must use shared state-access helpers instead of ad-hoc state reads.
- FR-2: Edge processing must be decomposed into named helpers with explicit per-edge context.
- FR-3: A shared unification core with explicit strategy configuration must exist and be used by Normalize, Solve, and Presolution.
- FR-4: Elaboration entry points must accept structured config or env records instead of long parameter lists.
- FR-5: Graph and type definitions must be split into smaller submodules, re-exported to preserve the public API.
- FR-6: Paper-faithfulness documentation must be updated or expanded as required by module changes.

## Non-Goals (Out of Scope)

- No algorithmic changes or behavioral changes to inference or elaboration.
- No performance optimizations unless required for parity.
- No UI or CLI changes.
- No new features or language extensions unrelated to refactor.

## Design Considerations

- Maintain a stable public API via re-exports even if modules move or split.
- Prefer small, named helpers and Note blocks for paper-sensitive logic.
- Keep new types (EdgeCtx, ElabConfig, ElabEnv) minimal and explicit.

## Technical Considerations

- Update mlf2.cabal module lists whenever files move or new modules are added.
- Keep warnings clean with -Wall.
- Follow presolution guidelines: new state access must go through StateAccess or Ops.
- Tests must only import exposed modules; re-export internal helpers if tests need them.

## Success Metrics

- Cabal build all and cabal test pass without warnings.
- Reduced ad-hoc presolution state access (verified by review or ripgrep checks).
- Paper-map coverage for all six pipeline phases and witness translation paths.
- Easier navigation: large modules reduced in size and responsibilities clearly separated.

## Open Questions

- Preferred naming for the shared unification core module.
- Whether docs/paper-map.md should include thesis page numbers or only section and figure references.
- Whether docs/phase-notes.md should include a lightweight audit checklist for future reviews.
