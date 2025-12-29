# Implementation Plan

- [x] 1. Add explicit bottom node to the constraint graph
  - Steps:
    - Extend `TyNode` with `TyBottom` and update `structuralChildren`.
    - Ensure total pattern matches in `Normalize`, `Solve`, and validation
      handle the new constructor.
  - Files: `src/MLF/Constraint/Types.hs`, `src/MLF/Constraint/Normalize.hs`,
    `src/MLF/Constraint/Solve.hs`
  - Tests: compile-only for this step.
  - Verification: `rg -n "TyBottom" src/MLF/Constraint`
  - _Requirements: 1.3_

- [x] 2. Implement elimination graph rewrite
  - Steps:
    - Add `rewriteEliminatedBinders` to rewrite node references and binding
      edges based on the eliminated-binder set.
    - Substitute eliminated binders with their bounds or the bottom node.
    - Clear `cEliminatedVars` after rewrite.
  - Files: `src/MLF/Constraint/Solve.hs`
  - Tests: N/A (covered in task 4).
  - Verification: `rg -n "rewriteEliminatedBinders" src/MLF/Constraint`
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 3. Wire rewrite into the pipeline and drop marker usage
  - Steps:
    - Apply the rewrite pass after solve (before elaboration).
    - Remove `cEliminatedVars` checks in reification/generalization.
  - Files: `src/MLF/Constraint/Solve.hs`, `src/MLF/Elab/Reify.hs`,
    `src/MLF/Elab/Generalize.hs`
  - Tests: N/A (covered in task 4).
  - Verification: `rg -n "cEliminatedVars|isEliminatedVar" src/MLF/Elab`
  - _Requirements: 2.1, 2.2_

- [x] 4. Add regression coverage for elimination rewrite
  - Steps:
    - Add a unit test that eliminates a binder via Merge/Weaken and checks
      Q(n) enumeration excludes it after rewrite.
    - Add a regression for unbounded elimination (bottom substitution) in
      elaboration output.
  - Files: `test/ElaborationSpec.hs`, `test/ConstraintGenSpec.hs`
  - Tests: `cabal test --test-show-details=direct`
  - Verification: `rg -n "elimination rewrite|eliminated" test`
  - _Requirements: 3.1, 3.2_

- [x] 5. Update documentation
  - Steps:
    - Document the elimination rewrite and the removal of the marker in
      `implementation_notes.md`.
  - Files: `implementation_notes.md`
  - Tests: N/A (doc-only).
  - Verification: `rg -n "elimination rewrite" implementation_notes.md`
  - _Requirements: 2.1, 3.2_
