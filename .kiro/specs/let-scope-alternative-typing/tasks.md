# Implementation Plan

- [ ] 1. Audit let scoping and identify required changes
  - Steps:
    - Document the current scoping behavior for lets in constraint generation.
    - Map the thesis alternative scoping rule onto existing structures.
  - Files: `src/MLF/Frontend/ConstraintGen/*`
  - Tests: N/A (documentation)
  - Verification: `rg -n "ELet|let" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1_

- [ ] 2. Implement alternative let scoping
  - Steps:
    - Encode the alternative scoping rule in constraint generation or
      presolution as appropriate.
    - Thread the scoping choice into generalization or elaboration.
  - Files: `src/MLF/Frontend/ConstraintGen/*`, `src/MLF/Elab/Generalize.hs`
  - Tests: new let-scoping regressions
  - Verification: `rg -n "scope" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1, 1.2_

- [ ] 3. Validate translatable presolutions
  - Steps:
    - Add a check for translatability conditions from Chapter 15.2.7.
    - Surface a structured error when violated.
  - Files: `src/MLF/Constraint/Presolution/*`
  - Tests: presolution validation regression
  - Verification: `rg -n "translatable" src/MLF/Constraint/Presolution -S`
  - _Requirements: 2.1, 2.2_

- [ ] 4. Add tests
  - Steps:
    - Add let-scoping regression tests.
    - Add a translatability validation test.
  - Files: `test/ElaborationSpec.hs`
  - Tests: `cabal test --test-options=--match=let-scope`
  - Verification: `rg -n "let" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_
