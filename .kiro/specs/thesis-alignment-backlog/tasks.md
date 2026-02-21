# Implementation Plan

- [x] 1. Create thesis-alignment backlog spec set
  - Steps:
    - Add `.kiro/specs/thesis-alignment-backlog/requirements.md` with EARS
      acceptance criteria and scope.
    - Add `.kiro/specs/thesis-alignment-backlog/design.md` with a high-level
      inventory and backlog items.
    - Add `.kiro/specs/thesis-alignment-backlog/tasks.md` with verifiable tasks.
  - Files:
    - `.kiro/specs/thesis-alignment-backlog/requirements.md`
    - `.kiro/specs/thesis-alignment-backlog/design.md`
    - `.kiro/specs/thesis-alignment-backlog/tasks.md`
  - Tests: N/A (spec authoring).
  - Verification:
    - `test -f .kiro/specs/thesis-alignment-backlog/requirements.md`
    - `test -f .kiro/specs/thesis-alignment-backlog/design.md`
    - `test -f .kiro/specs/thesis-alignment-backlog/tasks.md`
  - _Requirements: 1.1, 1.2, 2.1, 3.1_

- [x] 2. Create new spec stubs for P0 backlog items
  - Steps:
    - Add `.kiro/specs/inert-locked-filtering`.
    - Add `.kiro/specs/leftmost-lowermost-order`.
  - Files: new spec directories with requirements/design/tasks.
  - Tests: N/A (spec authoring).
  - Verification:
    - `test -d .kiro/specs/inert-locked-filtering`
    - `test -d .kiro/specs/leftmost-lowermost-order`
  - _Requirements: 1.2, 2.3, 3.1_

- [x] 3. Create new spec stubs for P1 backlog items
  - Steps:
    - Add `.kiro/specs/translation-contexts`.
    - Add `.kiro/specs/let-scope-alternative-typing`.
    - Add `.kiro/specs/syntactic-graphic-translation`.
    - Add `.kiro/specs/constraint-similarity-abstraction`.
    - Add `.kiro/specs/generalized-unification`.
  - Files: new spec directories with requirements/design/tasks.
  - Tests: N/A (spec authoring).
  - Verification:
    - `test -d .kiro/specs/translation-contexts`
    - `test -d .kiro/specs/let-scope-alternative-typing`
    - `test -d .kiro/specs/syntactic-graphic-translation`
    - `test -d .kiro/specs/constraint-similarity-abstraction`
    - `test -d .kiro/specs/generalized-unification`
  - _Requirements: 1.2, 2.3, 3.1_

- [ ] 4. Create new spec stubs for P2 backlog items
  - Steps:
    - Add `.kiro/specs/constraint-simplification-rules`.
    - Add `.kiro/specs/emlf-imlf-translation`.
  - Files: new spec directories with requirements/design/tasks.
  - Tests: N/A (spec authoring).
  - Verification:
    - `test -d .kiro/specs/constraint-simplification-rules`
    - `test -d .kiro/specs/emlf-imlf-translation`
  - _Requirements: 1.2, 2.3, 3.1_
  - **Deferred (2026-02-22):** P2 items are low priority. Registered as
    DEV-P2-SPEC-STUBS-DEFERRED in docs/thesis-deviations.yaml.

- [ ] 5. Record xmlf conflicts as they are discovered
  - Steps:
    - Add conflict notes to `.kiro/specs/thesis-alignment-backlog/design.md`.
  - Files: `.kiro/specs/thesis-alignment-backlog/design.md`.
  - Tests: N/A (documentation updates).
  - Verification: `rg -n "Conflict" .kiro/specs/thesis-alignment-backlog/design.md`
  - _Requirements: 1.3, 3.2_
  - **Deferred (2026-02-22):** Ongoing documentation task. No conflicts
    discovered yet; will be recorded as they arise during future work.
