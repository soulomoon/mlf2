# Implementation Plan

- [ ] 1. Add Note [Constraint simplification: Var-Abs (Ch 12.4.1)]
  - Document lambda parameter monomorphic binding as implicit Var-Abs.
  - Document degenerate forall handling in expansion as implicit Var-Abs.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`,
    `src/MLF/Constraint/Presolution/Expansion.hs`
  - Requirements: 1.1
  - Verification: `rg "Var-Abs" src/`

- [ ] 2. Add Note [Constraint simplification: Var-Let (Ch 12.4.1)]
  - Document `dropTrivialSchemeEdges` as implicit Var-Let.
  - Files: `src/MLF/Constraint/Presolution/Base.hs`
  - Requirements: 1.2
  - Verification: `rg "Var-Let" src/`

- [ ] 3. Add Note [ML-Extrude omitted (Ch 12.4.2)]
  - Document why ML-Extrude is intentionally not implemented for MLF.
  - Files: `src/MLF/Constraint/Presolution/Base.hs` or nearby
  - Requirements: 1.3
  - Verification: `rg "ML-Extrude" src/`

- [ ] 4. Add Var-Abs test
  - Test that lambda parameters produce monomorphic bindings (no gen node).
  - Files: `test/PipelineSpec.hs` or `test/ElaborationSpec.hs`
  - Requirements: 2.1
  - Verification: `cabal test --test-show-details=direct`

- [ ] 5. Add Var-Let test
  - Test that trivial let-binding expansion is identity.
  - Files: `test/PipelineSpec.hs` or `test/ElaborationSpec.hs`
  - Requirements: 2.2
  - Verification: `cabal test --test-show-details=direct`
