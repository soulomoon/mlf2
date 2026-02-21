# Implementation Plan

- [ ] 1. Define individual simplification rules
  - Steps:
    - Add a new module with each Chapter 13 simplification rule as a function.
    - Each rule takes a Constraint and returns Maybe Constraint.
  - Files: `src/MLF/Constraint/Simplification.hs`
  - Tests: N/A (compile-only)
  - Verification: `rg -n "simplif" src/MLF/Constraint/Simplification.hs`
  - _Requirements: 1.1, 1.2_

- [ ] 2. Integrate rules into presolution
  - Steps:
    - Replace implicit simplification with explicit rule application.
    - Apply rules in fixed-point until no rule is applicable.
  - Files: `src/MLF/Constraint/Simplification.hs`, `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests: existing presolution tests should still pass
  - Verification: `rg -n "simplif" src/MLF/Constraint/Presolution/Driver.hs`
  - _Requirements: 1.1_

- [ ] 3. Add tests
  - Steps:
    - Add per-rule unit tests showing solution preservation.
    - Add negative tests for non-applicable rules.
  - Files: `test/SimplificationSpec.hs` (new)
  - Tests: `cabal test --test-options=--match=simplif`
  - Verification: `rg -n "simplif" test -S`
  - _Requirements: 2.1, 2.2_
