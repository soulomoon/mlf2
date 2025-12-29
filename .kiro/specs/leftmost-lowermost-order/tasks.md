# Implementation Plan

- [x] 1. Audit and align order key computation
  - Steps:
    - Compare `orderKeysFromRootWith` to the thesis definition of <P.
    - Adjust `OrderKey` computation if any mismatch is found.
  - Files:
    - `src/MLF/Util/OrderKey.hs`
  - Tests:
    - Add or update unit tests that reflect the thesis ordering.
  - Verification:
    - `rg -n "OrderKey" src/MLF/Util/OrderKey.hs`
  - _Requirements: 1.1, 1.2_

- [x] 2. Enforce aligned ordering at call sites
  - Steps:
    - Ensure Phi translation and merge-direction checks use the aligned order.
    - Remove any fallback ordering that contradicts <P.
  - Files:
    - `src/MLF/Elab/Phi.hs`
    - `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Regression test that a known ordering case translates correctly.
  - Verification:
    - `rg -n "orderKeysFromRoot" src/MLF/Elab/Phi.hs`
  - _Requirements: 2.1, 2.2_

- [x] 3. Add ordering tests
  - Steps:
    - Add a dedicated spec (or extend `PresolutionSpec`) to cover ordering.
    - Include a shared-node test and a branching DAG test.
  - Files:
    - `test/OrderSpec.hs` (or `test/PresolutionSpec.hs`)
  - Tests:
    - `cabal test --test-show-details=direct --test-options=--match=order`
  - Verification:
    - `rg -n "order" test`
  - _Requirements: 3.1, 3.2_

- [x] 4. Full test run
  - Verification: `cabal test --test-show-details=direct`
  - _Requirements: 3.1, 3.2_
