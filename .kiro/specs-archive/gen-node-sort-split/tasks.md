# Implementation Plan

- [x] 1. Data model split (types + constraint fields)
  - Steps:
    - Introduce `GenNodeId`, `GenNode`, and `NodeRef` in `MLF.Constraint.Types`.
    - Add `cGenNodes :: IntMap GenNode` to the constraint.
    - Update constructors (`ConstraintGen`, `Presolution`, tests) to populate it.
  - Files: `src/MLF/Constraint/Types.hs`, `src/MLF/Frontend/ConstraintGen/*`, `test/SpecUtil.hs`
  - Tests: compile-only
  - _Requirements: 1.1, 1.2_

- [x] 2. Binding tree migration to `NodeRef`
  - Steps:
    - Update `BindParents` to use `NodeRef` keys and values.
    - Migrate binding-tree operations (`Binding.Tree`, `Binding.Adjustment`, `Binding.GraphOps`).
    - Enforce gen-node binding constraints (gen-on-gen only).
  - Files: `src/MLF/Binding/*`, `src/MLF/Constraint/Types.hs`
  - Tests: binding-tree unit tests
  - _Requirements: 3.1, 3.2_

- [x] 3. Rootedness enforcement
  - Steps:
    - Create a single root gen node in Phase 1.
    - Remove synthetic root insertion in `Constraint.Root`.
    - Add validation checks for rootedness violations.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Constraint/Root.hs`
  - Tests: new rootedness tests
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 4. Elaboration alignment
  - Steps:
    - Recompute named nodes from gen-node bindings only.
    - Update context computation to follow thesis gen-node rules.
  - Files: `src/MLF/Elab/Reify.hs`, `src/MLF/Elab/Phi.hs`
  - Tests: elaboration/context tests
  - _Requirements: 4.1, 4.2_

- [x] 5. Cleanup legacy encoding
  - Steps:
    - Stop treating `TyForall` as gen nodes; remove synthetic root handling.
    - Remove transitional compatibility fields/helpers.
  - Files: `src/MLF/Constraint/Types.hs`, `src/MLF/Frontend/ConstraintGen/*`
  - Tests: full `cabal test`
  - _Requirements: 1.1, 2.2, 3.1_
