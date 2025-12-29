# Implementation Plan

- [ ] 1. Audit current unification logic
  - Steps:
    - Identify current entry points for unification in presolution/solve.
    - Map existing cases to Chapter 7.6 rules.
  - Files: `src/MLF/Constraint/Presolution/Unify.hs`, `src/MLF/Constraint/Unify/Decompose.hs`
  - Tests: N/A (analysis)
  - Verification: `rg -n "unify" src/MLF/Constraint -S`
  - _Requirements: 1.1_

- [ ] 2. Implement generalized unification rules
  - Steps:
    - Add rule handling for generalized cases from Chapter 7.6.
    - Introduce a specific error for generalized failures.
  - Files: `src/MLF/Constraint/Presolution/Unify.hs`, `src/MLF/Constraint/Types.hs`
  - Tests: new unit tests
  - Verification: `rg -n "Generalized" src/MLF/Constraint -S`
  - _Requirements: 1.1, 1.2_

- [ ] 3. Integrate into presolution
  - Steps:
    - Ensure the generalized path runs before standard unification.
    - Preserve existing behavior when rules do not apply.
  - Files: `src/MLF/Constraint/Presolution/Unify.hs`
  - Tests: regression tests
  - Verification: `rg -n "generalized" src/MLF/Constraint/Presolution/Unify.hs`
  - _Requirements: 2.1, 2.2_

- [ ] 4. Add tests
  - Steps:
    - Add generalized unification tests and a regression case.
  - Files: `test/SolveSpec.hs` or `test/PresolutionSpec.hs`
  - Tests: `cabal test --test-options=--match=generalized-unification`
  - Verification: `rg -n "generalized" test -S`
  - _Requirements: 3.1, 3.2_
