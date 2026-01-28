# Implementation Plan

- [x] 1. Define or adjust context representation
  - Steps:
    - Audit current `ContextStep` usage and extend if needed.
    - Document mapping to Chapter 15.3 definitions.
  - Files: `src/MLF/Elab/Types.hs`
  - Tests: compile-only
  - Verification: `rg -n "ContextStep" src/MLF/Elab`
  - _Requirements: 1.1_

- [x] 2. Implement context computation
  - Steps:
    - Add context computation for under/inside paths from root to binder.
    - Ensure canonicalization and binding-path queries are respected.
  - Files: `src/MLF/Elab/Phi.hs`
  - Tests: unit tests for context computation
  - Verification: `rg -n "context" src/MLF/Elab/Phi.hs`
  - _Requirements: 1.1, 1.2_

- [x] 3. Apply contexts in Phi translation
  - Steps:
    - Thread computed contexts into non-spine Raise/Insert operations.
    - Replace ad-hoc ordering with context-based steps.
  - Files: `src/MLF/Elab/Phi.hs`
  - Tests: translation regression tests
  - Verification: `rg -n "non-spine" src/MLF/Elab/Phi.hs`
  - _Requirements: 2.1, 2.2_

- [x] 4. Add tests
  - Steps:
    - Add regression cases in `test/ElaborationSpec.hs` for context derivation.
    - Add Phi translation tests that require inside-bound contexts.
  - Files: `test/ElaborationSpec.hs`
  - Tests: `cabal test --test-options=--match=Context`
  - Verification: `rg -n "context" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_
