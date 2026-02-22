# Implementation Plan

- [ ] 1. Crosswalk and evidence capture
  - [ ] 1.1 Map paper grammar to AST constructors and record evidence
    - Update documentation to explicitly list paper grammar vs `Expr` constructors.
    - Files: `docs/paper-map.md`, `implementation_notes.md`
    - _Requirements: 1.1_
    - **Verification:** manual doc review
  - [ ] 1.2 Document extra surface constructs
    - Record `ELetAnn`, `ELit`, and `EVarRaw` as extensions or internal-only forms.
    - Files: `docs/paper-map.md`, `implementation_notes.md`
    - _Requirements: 1.2_
    - **Verification:** manual doc review

- [ ] 2. Annotation alignment decisions
  - [ ] 2.1 Decide how to model kappa annotations
    - Either document the mapping (free vars as existentials) or introduce an explicit
      annotation AST for kappa if exactness is required.
    - Files: `MLF.Frontend.Syntax`, `docs/paper-map.md`
    - _Requirements: 2.1_
    - **Verification:** doc update or code review
  - [ ] 2.2 Align or document coercion desugaring
    - If exact match is required, desugar `EAnn`/`ELamAnn` to coercion functions;
      otherwise document why explicit annotation nodes are retained.
    - Files: `MLF.Frontend.Desugar`, `docs/paper-map.md`
    - _Requirements: 1.3_
    - **Verification:** tests or doc review

- [ ] 3. Tests (if behavior changes)
  - [ ] 3.1 Add or adjust tests for annotation sugar
    - Ensure `ConstraintGenSpec` covers the chosen representation (explicit or desugared).
    - Files: `test/ConstraintGenSpec.hs`
    - _Requirements: 1.3, 2.1_
    - **Verification:** `cabal test`
