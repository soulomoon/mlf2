# Implementation Plan

- [x] 1. Suppress `OpWeaken` when a later `ExpForall` exists
  - Steps:
    - Flatten `ExpCompose` into an ordered list of expansion components.
    - Compute a suffix flag (`hasForallLater`) for each component.
    - Omit `OpWeaken` emission for `ExpInstantiate` when `hasForallLater` is
      true; keep `OpGraft` and `OpMerge` ordering.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - Verification: `rg -n "hasForallLater|OpWeaken" src/MLF/Constraint/Presolution/Witness.hs`
  - _Requirements: 1.1, 2.1_

- [x] 2. Confirm regressions for quantifier preservation
  - Steps:
    - Ensure `PresolutionSpec` ordering test for `StepIntro` still passes.
    - Ensure the bounded aliasing baseline reports `∀a. a -> a -> a`.
  - Files: `test/PresolutionSpec.hs`, `test/ElaborationSpec.hs`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - Verification: `rg -n "bounded aliasing|StepIntro" test/ElaborationSpec.hs test/PresolutionSpec.hs`
  - _Requirements: 1.3, 2.1, 2.2_
