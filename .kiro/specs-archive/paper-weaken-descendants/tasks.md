# Implementation Plan

- [x] 1. Update Weaken reordering to use strict descendants
  - Steps:
    - Remove any “self-in-descendants” handling in `reorderWeakenWithEnv` so the anchor/descendant set excludes `n` itself.
    - Add a brief comment referencing `papers/xmlf.txt` condition (5) and stating that “below n” means strict descendants.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "below n|strict descendant" src/MLF/Constraint/Presolution/Witness.hs`
  - _Requirements: 1.1, 2.2_

- [x] 2. Align Weaken normalization tests with strict-descendant semantics
  - Steps:
    - Update the “pushes Weaken after other ops on the binder” test to assert that Weaken is only moved after strict descendants, not same-binder ops.
    - Add/adjust a regression test that `Weaken(n)` is not forced past an op targeting `n` when there are no descendants in the interior set.
  - Files: `test/PresolutionSpec.hs`
  - Tests: Hspec unit tests
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Witness normalization/'`
  - _Requirements: 1.2, 1.3, 2.1_

- [x] 3. Full test run
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - _Requirements: 1.1, 1.2, 1.3_
