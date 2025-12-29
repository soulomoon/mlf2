# Implementation Plan

- [ ] 1. Define similarity/abstraction relations
  - Steps:
    - Add a new module describing similarity and abstraction steps.
    - Decide on any additional metadata required in `Constraint`.
  - Files: `src/MLF/Constraint/Similarity.hs`, `src/MLF/Constraint/Types.hs`
  - Tests: N/A (compile-only)
  - Verification: `rg -n "Similarity|Abstraction" src/MLF/Constraint -S`
  - _Requirements: 1.1, 2.1_

- [ ] 2. Implement canonicalization under similarity
  - Steps:
    - Implement a normalization function that produces a canonical form.
    - Ensure stability under similarity steps.
  - Files: `src/MLF/Constraint/Similarity.hs`
  - Tests: unit tests for canonicalization
  - Verification: `rg -n "canonical" src/MLF/Constraint/Similarity.hs`
  - _Requirements: 1.2_

- [ ] 3. Integrate similarity checks
  - Steps:
    - Use similarity-aware comparison where constraints are validated.
  - Files: `src/MLF/Constraint/Solve.hs`
  - Tests: regression tests for similarity equivalence
  - Verification: `rg -n "similar" src/MLF/Constraint/Solve.hs`
  - _Requirements: 1.1, 2.2_

- [ ] 4. Add tests
  - Steps:
    - Add tests for similarity and abstraction equivalence.
  - Files: `test/ConstraintSpec.hs` (new) or `test/SolveSpec.hs`
  - Tests: `cabal test --test-options=--match=similarity`
  - Verification: `rg -n "similarity" test -S`
  - _Requirements: 3.1, 3.2_
