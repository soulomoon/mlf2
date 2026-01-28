# Implementation Plan

- [x] 1. Establish shared recursion-schemes helpers
  - Steps: add or extend `MLF.Util.RecursionSchemes` with effectful folds and
    small convenience wrappers for Elab types.
  - Files: `src/MLF/Util/RecursionSchemes.hs`, `mlf2.cabal`
  - Tests: N/A (covered by task 5).
  - Verification: `rg -n "cataM|cataEither|cataMaybe" src/MLF/Util/RecursionSchemes.hs`
  - _Requirements: 2.1, 2.2_

- [x] 2. Convert Elab.Types traversals using advanced schemes
  - Steps: refactor safe traversals (e.g., inline-bounds display, scheme
    conversions, occurrence/free-vars helpers) to para/apo/zygo/histo where
    needed.
  - Files: `src/MLF/Elab/Types.hs`
  - Tests: N/A (covered by task 5).
  - Verification: `rg -n "para|apo|zygo|histo" src/MLF/Elab/Types.hs`
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 3. Convert remaining Elab/Frontend structural traversals
  - Steps: refactor safe explicit recursions in Elab/Frontend modules using
    the appropriate schemes, preserving binder behavior.
  - Files: `src/MLF/Elab/*.hs`, `src/MLF/Frontend/*.hs`
  - Tests: N/A (covered by task 5).
  - Verification: `rg -n "fold|cata|para|apo|zygo|histo" src/MLF/Elab src/MLF/Frontend`
  - _Requirements: 1.1, 1.2_

- [x] 4. Add effectful traversal conversions
  - Steps: identify Maybe/Either/Monad traversals and switch them to effectful
    folds using the shared helpers.
  - Files: `src/MLF/Elab/*.hs`, `src/MLF/Frontend/*.hs`, `src/MLF/Util/RecursionSchemes.hs`
  - Tests: N/A (covered by task 5).
  - Verification: `rg -n "cataM|cataEither|cataMaybe" src/MLF/Elab src/MLF/Frontend`
  - _Requirements: 2.1_

- [x] 5. Validate with existing tests
  - Steps: run the test suite and fix any regressions.
  - Files: N/A
  - Tests: `cabal test`
  - Verification: test run completes without failures.
  - _Requirements: 4.1_
