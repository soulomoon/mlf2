# Implementation Plan

- [x] 1. Add a witness-step type and update EdgeWitness storage
  - Steps:
    - Introduce `InstanceStep` (or equivalent) in `MLF.Constraint.Types` with
      `StepOmega InstanceOp` and `StepIntro` variants.
    - Update `EdgeWitness` to store a step list (e.g., `ewSteps :: [InstanceStep]`)
      and remove or deprecate `ewForallIntros`.
  - Files: `src/MLF/Constraint/Types.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "InstanceStep|StepIntro|ewSteps" src/MLF/Constraint/Types.hs`
  - _Requirements: 1.1, 2.1_

- [x] 2. Emit StepIntro in presolution witnesses
  - Steps:
    - Update `witnessFromExpansion` to emit `StepIntro` when processing ExpForall.
    - Preserve ExpCompose ordering when building the step list.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests: unit test in `test/PresolutionSpec.hs` for ExpCompose ordering
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /StepIntro/'`
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 3. Normalize Omega segments without reordering StepIntro
  - Steps:
    - Implement a helper that splits steps into contiguous Omega segments.
    - Run `normalizeInstanceOpsFull` on each Omega segment and reassemble with
      StepIntro boundaries preserved.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`, `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests: unit test ensuring StepIntro order is preserved while Omega ops are normalized
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /StepIntro.*normalized/'`
  - _Requirements: 2.2, 3.1, 3.2_

- [x] 4. Update Phi translation to consume step lists
  - Steps:
    - Translate `StepIntro` to `InstIntro` inline in the instantiation sequence.
    - Translate `StepOmega` using existing Omega Phi logic.
  - Files: `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Types.hs` (if needed)
  - Tests: regression test for interleaved StepIntro and Omega ops
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Phi.*StepIntro/'`
  - _Requirements: 2.1, 2.2_

- [x] 5. Clean up ewForallIntros usage and update call sites
  - Steps:
    - Remove `ewForallIntros` references from presolution and elaboration.
    - Update any helper functions or tests that assume a suffix-only intro count.
  - Files: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Elab/Phi.hs`, tests
  - Tests: full test suite
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - _Requirements: 1.3, 2.2, 3.3_

- [x] 6. Emit StepIntro per ForallSpec binder count
  - Steps:
    - Update `witnessFromExpansion` so each `ForallSpec` emits `fsBinderCount`
      `StepIntro` steps.
    - Add a regression test for multi-binder `ForallSpec`.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`, `test/PresolutionSpec.hs`
  - Tests: Hspec unit tests
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /StepIntro/'`
  - _Requirements: 1.1, 1.2_
