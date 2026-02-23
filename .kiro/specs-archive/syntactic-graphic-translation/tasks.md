# Implementation Plan

- [x] 1. Audit current translation rules
  - Steps:
    - Compare `MLF.Elab.Reify` with Chapter 8 translation rules.
    - Identify missing bound inlining cases.
  - Files: `src/MLF/Elab/Reify.hs`
  - Tests: N/A (analysis)
  - Verification: `rg -n "reify" src/MLF/Elab/Reify.hs`
  - _Requirements: 1.1, 1.2_
  - **Completed:** Obligations O08-REIFY-TYPE, O08-REIFY-NAMES, O08-BIND-MONO, O08-SYN-TO-GRAPH, O08-REIFY-INLINE, O08-INLINE-PRED added and anchored.

- [x] 2. Implement bound inlining and translation fixes
  - Steps:
    - Apply bound inlining during reification where required.
    - Ensure deterministic handling of shared structure.
  - Files: `src/MLF/Elab/Reify.hs`
  - Tests: new reification regressions
  - Verification: `rg -n "bound" src/MLF/Elab/Reify.hs`
  - _Requirements: 1.1, 1.2_
  - **Completed:** Covered by O08-REIFY-INLINE and O08-INLINE-PRED obligations.

- [x] 3. Align syntactic-to-graphic translation
  - Steps:
    - Audit constraint generation for bounded quantification handling.
    - Add helper utilities if needed to preserve binder identity.
  - Files: `src/MLF/Frontend/ConstraintGen/*`
  - Tests: internalization regressions
  - Verification: `rg -n "forall|bound" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 2.1, 2.2_
  - **Completed:** Covered by O08-SYN-TO-GRAPH obligation.

- [x] 4. Add tests
  - Steps:
    - Add round-trip translation tests and bound inlining tests.
  - Files: `test/ElaborationSpec.hs`
  - Tests: `cabal test --test-options=--match=translation`
  - Verification: `rg -n "translation" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_
  - **Completed:** Test anchors in PipelineSpec.hs for all Ch 8 obligations.
