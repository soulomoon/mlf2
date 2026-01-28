# Implementation Plan

- [x] 1. Ensure application constraint generation emits and records two instantiation edges
  - Steps:
    - Confirm `EApp` allocates a fresh arrow `(d -> r)` and emits instantiation
      edges from the function to the arrow and from the argument to `d`.
    - If missing or mismatched, update `AApp` construction to store both edge
      ids and keep the Figure 7 note in sync.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Frontend/ConstraintGen/Types.hs`
  - Tests: constraint generation regression
  - Verification: `rg -n "AApp|addInstEdge" src/MLF/Frontend/ConstraintGen/Translate.hs`
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2. Apply instantiation to both sides during elaboration
  - Steps:
    - Ensure elaboration reifies instantiations for both `funEid` and `argEid`.
    - Wrap each subterm with `ETyInst` when its instantiation is non-identity.
  - Files: `src/MLF/Elab/Elaborate.hs`
  - Tests: elaboration regression
  - Verification: `rg -n "funEid|argEid|ETyInst" src/MLF/Elab/Elaborate.hs`
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 3. Add regression tests for dual instantiation
  - Steps:
    - Add a `ConstraintGenSpec` test that `EApp` emits two instantiation edges
      and that `AApp` records both edge ids.
    - Add an elaboration or pipeline test that `let id = \x. x in id id`
      elaborates to an application with `ETyInst` on both sides.
  - Files: `test/ConstraintGenSpec.hs`, `test/ElaborationSpec.hs` (or `test/PipelineSpec.hs`)
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /dual instantiation|id id/'`
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /dual instantiation|id id/'`
  - _Requirements: 1.1, 2.1, 3.1, 3.2_
