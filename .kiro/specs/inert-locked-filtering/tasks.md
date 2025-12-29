# Implementation Plan

- [x] 1. Add inert and inert-locked classification helpers
  - Steps:
    - Implement `inertNodes` and `inertLockedNodes` utilities and expose them
      to presolution/translation.
  - Files:
    - `src/MLF/Constraint/Inert.hs`
    - Call sites in `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests:
    - Unit test for a minimal constraint with a known inert-locked node.
  - Verification:
    - `rg -n "inertLockedNodes" src/MLF/Constraint`
  - _Requirements: 1.1, 1.2_

- [x] 2. Add inert-locked weakening pass for presolutions
  - Steps:
    - Implement a pass that weakens inert-locked nodes to rigid bindings when
      possible (Lemma 15.2.4).
    - Emit a structured error if the pass cannot produce a translatable
      presolution.
  - Files:
    - `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests:
    - Regression test for a presolution with inert-locked nodes that becomes
      translatable after weakening.
  - Verification:
    - `rg -n "inert-locked" src/MLF/Constraint`
  - _Requirements: 2.2_

- [x] 3. Filter witness operations on inert-locked nodes
  - Steps:
    - Extend `OmegaNormalizeEnv` with an inert-locked set.
    - Filter ops targeting inert-locked nodes before validation/translation.
  - Files:
    - `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Unit test that filtered witnesses contain no inert-locked targets.
  - Verification:
    - `rg -n "inertLocked" src/MLF/Constraint/Presolution/Witness.hs`
  - _Requirements: 2.1, 3.2_

- [x] 4. Translation integration
  - Steps:
    - Ensure Phi translation consumes the filtered witness list.
  - Files:
    - `src/MLF/Elab/Phi.hs`
  - Tests:
    - Regression test that translation succeeds on an inert-locked scenario.
  - Verification:
    - `rg -n "inert-locked" src/MLF/Elab/Phi.hs`
  - _Requirements: 2.1, 3.1_

- [x] 5. Full test run
  - Verification: `cabal test --test-show-details=direct`
  - _Requirements: 3.1, 3.2_

- [x] 6. Align inert-locked definition to the thesis flag-path rule
  - Steps:
    - Require inert nodes to be flexibly bound and under a rigid ancestor.
    - Remove any extra gating beyond inert + flex + rigid-ancestor checks.
  - Files:
    - `src/MLF/Constraint/Inert.hs`
  - Tests:
    - Regression test where a flex + rigid-ancestor node is inert-locked.
  - Verification:
    - `rg -n "inertLockedNodes" src/MLF/Constraint/Inert.hs`
  - _Requirements: 1.1, 1.2_

- [x] 7. Update inert-locked regression tests
  - Steps:
    - Adjust inert-locked tests to match the thesis definition.
  - Files:
    - `test/PresolutionSpec.hs`
  - Tests:
    - Existing inert-locked detection cases
  - Verification:
    - `rg -n "Inert-locked detection" test/PresolutionSpec.hs`
  - _Requirements: 1.1, 3.1_

- [x] 8. Re-run full test suite after alignment changes
  - Verification: `cabal test --test-show-details=direct`
  - _Requirements: 3.1, 3.2_
