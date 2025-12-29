# Implementation Plan

- [x] 1. Inline rigid TyVars during reification
  - Steps: update `reifyWith` to detect rigid binding edges and inline bounds
    (or ⊥ when missing).
  - Files: `src/MLF/Elab/Reify.hs`
  - Tests: N/A (covered in task 3).
  - Verification: `rg -n "BindRigid" src/MLF/Elab/Reify.hs`
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2. Extend non-Forall generalization reachability and rigid filtering
  - Steps: add a reachability helper that follows rigid bounds and update
    `boundAtScope` to require flexible binding paths.
  - Files: `src/MLF/Elab/Generalize.hs`
  - Tests: N/A (covered in task 3).
  - Verification: `rg -n "reachableWithRigidBounds|BindRigid" src/MLF/Elab/Generalize.hs`
  - _Requirements: 2.1, 2.2_

- [x] 3. Canonicalize variable bounds after solve
  - Steps: rewrite `cVarBounds`/`cEliminatedVars` through UF in `applyUFConstraint`.
  - Files: `src/MLF/Constraint/Solve.hs`
  - Tests: N/A (covered in task 4).
  - Verification: `rg -n "rewriteVarBounds|rewriteEliminated" src/MLF/Constraint/Solve.hs`
  - _Requirements: 1.1, 2.2_

- [x] 4. Add regression coverage for rigid-bound inlining
  - Steps: add a unit test that constructs a rigid TyVar bound to a flex var
    and asserts `generalizeAt` yields `∀a. a -> a`.
  - Files: `test/ElaborationSpec.hs`
  - Tests: `cabal test --test-show-details=direct`
  - Verification: `rg -n "rigid vars" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_

- [x] 5. Update docs for rigid-bound inlining
  - Steps: document the new behavior and reference the spec.
  - Files: `implementation_notes.md`, `.kiro/specs/generalization-fallback-retirement/design.md`
  - Tests: N/A (doc-only).
  - Verification: `rg -n "rigid" implementation_notes.md`
  - _Requirements: 2.2, 3.2_
