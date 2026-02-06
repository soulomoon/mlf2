# Implementation Plan

- [x] 1. Record the current (leftmost) let constraint and target shape
  - Steps:
    - Capture the current constraint shape for `let x = a in b` in notes/tests.
    - Write the target rightmost constraint shape (Fig. 15.2.6) explicitly.
  - Files: `.kiro/specs/let-scope-alternative-typing/design.md`
  - Tests: N/A (documentation)
  - Verification: `rg -n "ELet|let" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1_

- [x] 2. Implement alternative let scoping (rightmost constraint)
  - Steps:
    - Allocate a gen node for the let expression and a trivial scheme root
      (fresh type variable) bound under it.
    - Add the instantiation edge from `b`’s type to the trivial scheme root.
    - Use `AAnn` to carry the let-expression instantiation edge (no new `AnnExpr` fields).
    - Ensure the RHS `a` does not see `b`’s scope (no piggybacking).
  - Files: `src/MLF/Frontend/ConstraintGen/*`, `src/MLF/Frontend/ConstraintGen/Types.hs`
  - Tests: new let-scoping regressions
  - Verification: `rg -n "ELet|let" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1, 1.2_

- [x] 3. Enforce and validate translatable presolutions (Def. 15.2.10)
  - Steps:
    - Apply Theorem 15.2.11 order:
      (1) weaken inert-locked + app/abs arrow nodes;
      (2) weaken non-degenerate scheme roots and non-interior nodes bound on gen nodes;
      (3) re-run inert-locked weakening to eliminate newly introduced inert-locked nodes.
    - Skip degenerate scheme roots (scheme root no longer bound on its gen node).
    - Keep expansions unchanged; update witness normalization to reflect weakened nodes.
    - Validate and surface a structured error if any condition still fails.
  - Files: `src/MLF/Constraint/Presolution/*`
  - Tests: presolution validation regression
  - Verification: `rg -n "translatable" src/MLF/Constraint/Presolution -S`
  - _Requirements: 2.1, 2.2_

- [ ] 4. Thread let-expression schemes through elaboration/generalization
  - Steps:
    - Ensure elaboration treats the trivial scheme edge as identity (drop the AAnn wrapper).
    - Remove any presolution special-casing that forces identity unification for the trivial edge.
    - Confirm generalization scopes follow the new let-expression gen node layout.
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Elaborate.hs`
  - Tests: let-scoping regressions
  - Verification: `rg -n "ELet" src/MLF/Elab -S`
  - _Requirements: 1.2_

- [ ] 5. Add tests
  - Steps:
    - Add let-scoping regression tests (rightmost constraint + trivial scheme edge).
    - Update existing let tests for the extra trivial scheme edge and bounded schemes.
    - Add a translatability validation test (Def. 15.2.10 conditions).
  - Files: `test/ElaborationSpec.hs`, `test/PresolutionSpec.hs`
  - Tests: `cabal test --test-options=--match=let-scope`
  - Verification: `rg -n "let" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_
