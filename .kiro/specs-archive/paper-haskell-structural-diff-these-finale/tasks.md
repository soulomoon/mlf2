# Implementation Plan

- [ ] 1. Resolve non-expansion instantiation semantics
  - [ ] 1.1 Audit instantiation edges where the left-hand side is not a `TyExp` wrapper and determine paper-faithful behavior.
    - Files/components: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Constraint/Types.hs`
    - Test expectations: add a focused presolution test that exercises a non-expansion instantiation edge and verifies the expected witness or unification behavior.
    - _Requirements: 3.3_
    - **Verification:** `cabal test --test-show-details=direct`

- [ ] 2. Emit interior Raise operations in witnesses
  - [ ] 2.1 Extend presolution to record Raise operations for interior nodes during instantiation-edge solving and feed them into witness normalization.
    - Files/components: `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/Witness.hs`
    - Test expectations: add regression tests for Raise and RaiseMerge translation paths in `phiFromEdgeWitness`.
    - _Requirements: 3.4, 5.3_
    - **Verification:** `cabal test --test-show-details=direct`

- [ ] 3. Strengthen translatable-presolution coverage
  - [ ] 3.1 Add tests that assert `validateTranslatablePresolution` rejects inert-locked nodes and accepts the weakened form.
    - Files/components: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Inert.hs`, `test/PresolutionSpec.hs`
    - Test expectations: include at least one example with a non-interior gen-bound node and a rigid arrow node.
    - _Requirements: 4.1, 4.2_
    - **Verification:** `cabal test --test-show-details=direct`

- [ ] 4. Document alignment decisions and deviations
  - [ ] 4.1 Record the chosen semantics for non-expansion instantiation edges and Raise emission in alignment docs.
    - Files/components: `implementation_notes.md`, `incompatibility_report.md`
    - Test expectations: none (documentation only).
    - _Requirements: 1.1, 3.3, 3.4, 5.3_
    - **Verification:** manual review of documentation updates
