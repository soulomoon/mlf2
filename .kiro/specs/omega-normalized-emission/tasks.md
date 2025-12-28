# Implementation Plan

- [x] 1. Move elimination pruning out of the normalizer
  - Steps:
    - Remove the elimination-pruning pass (dropRedundantOps) from
      `normalizeInstanceOpsFull`.
    - Update comments to state that normalization enforces only paper conditions
      (1)–(5).
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "dropRedundantOps" src/MLF/Constraint/Presolution/Witness.hs` returns no matches
  - _Requirements: 1.1, 1.2_

- [x] 2. Remove emission-level elimination filtering
  - Steps:
    - Remove the elimination-based filtering inside `integratePhase2Ops` that
      drops Graft/Weaken when a later Merge/RaiseMerge appears.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests: unit test for emission output (see task 3)
  - Verification: `rg -n "extraElims|removeElimOps" src/MLF/Constraint/Presolution/Witness.hs` returns no matches
  - _Requirements: 2.1, 2.2_

- [x] 3. Update tests to reflect removal of pruning
  - Steps:
    - Replace the normalization test that expects pruning with a test that
      asserts `normalizeInstanceOpsFull` does not drop redundant ops.
    - Add a regression test that `integratePhase2Ops` preserves Graft/Weaken
      even when a later Merge/RaiseMerge eliminates the same binder.
  - Files: `test/PresolutionSpec.hs`
  - Tests: Hspec unit tests
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Witness normalization/'`
  - _Requirements: 3.1, 3.2_

- [x] 4. Full test run
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - _Requirements: 1.2, 2.1, 3.1_
