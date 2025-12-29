# Implementation Plan

- [ ] 1. Add Phase 7 error types and module scaffolding
  - Steps:
    - Introduce `TypeCheckError` (and optional `ReduceError`) in
      `src/MLF/Elab/Types.hs` or new modules.
    - Create stubs for `MLF.Elab.TypeCheck` and `MLF.Elab.Reduce` with minimal
      exports.
  - Files: `src/MLF/Elab/Types.hs`, `src/MLF/Elab/TypeCheck.hs`,
    `src/MLF/Elab/Reduce.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "TypeCheckError|typeCheck|reduce" src/MLF/Elab`
  - _Requirements: 1.3, 2.1_

- [ ] 2. Implement xMLF typechecking rules (Figure 4)
  - Steps:
    - Implement environment-based typechecking for `ElabTerm`.
    - Implement instantiation checking using `applyInstantiation`.
    - Ensure errors are descriptive and routed through `TypeCheckError`.
  - Files: `src/MLF/Elab/TypeCheck.hs`
  - Tests: unit tests for each core constructor
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Phase 7 typecheck/'`
  - _Requirements: 1.1, 1.2, 1.3_

- [ ] 3. Implement xMLF reduction semantics (Figure 5)
  - Steps:
    - Define `isValue` for the chosen evaluation strategy (CBV).
    - Implement `step` using evaluation contexts and the paper's rules.
    - Provide `normalize` as repeated stepping to a value or stuck term.
  - Files: `src/MLF/Elab/Reduce.hs`
  - Tests: reduction regression tests
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Phase 7 reduce/'`
  - _Requirements: 2.1, 2.2, 2.3_

- [ ] 4. Wire Phase 7 into the pipeline and public API
  - Steps:
    - Add a new entry point (e.g., `runPipelineElabChecked`) that runs Phase 1-6
      and then typechecks the elaborated term.
    - Re-export the entry point and helpers in `MLF.Elab.Pipeline` and
      `src-public/MLF/Pipeline.hs`/`src-public/MLF/API.hs`.
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Pipeline.hs`,
    `src-public/MLF/Pipeline.hs`, `src-public/MLF/API.hs`
  - Tests: integration test that runs the checked pipeline
  - Verification: `rg -n "runPipelineElabChecked|typeCheck" src src-public`
  - _Requirements: 3.1, 3.2, 3.3_

- [ ] 5. Add Phase 7 tests and wire them into the suite
  - Steps:
    - Add `test/TypeCheckSpec.hs` and `test/ReduceSpec.hs` (or extend
      `test/ElaborationSpec.hs`) with targeted unit tests.
    - Wire new test modules into `mlf2.cabal` and `test/Main.hs`.
    - Add a property/regression test for type preservation on a fixed set of
      terms.
  - Files: `test/TypeCheckSpec.hs`, `test/ReduceSpec.hs`, `test/Main.hs`,
    `mlf2.cabal`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Phase 7/'`
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct --test-options='--match /Phase 7/'`
  - _Requirements: 4.1, 4.2, 4.3_
