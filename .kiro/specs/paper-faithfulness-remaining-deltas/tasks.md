# Implementation Plan

- [ ] 1. Add constructor types to xMLF AST
  - [ ] 1.1 Extend `Ty`/`TyIF` with a constructor type (e.g., `TCon`) and update folds/pretty printers.
    - Files: `src/MLF/Types/Elab.hs`, `src/MLF/Elab/Types.hs`
    - Tests: add pretty/round‑trip cases in `test/TypeCheckSpec.hs`
    - _Requirements: 1.1_
    - **Verification:** `cabal test`
  - [ ] 1.2 Update instantiation/typechecking to allow constructor types in bounds and terms.
    - Files: `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `src/MLF/Elab/Reduce.hs`
    - Tests: new instantiation tests for `Cσ`.
    - _Requirements: 1.1, 1.2_
    - **Verification:** `cabal test`

- [ ] 2. Integrate quantifier reordering ϕR in elaboration
  - [ ] 2.1 Compute `Typ(a′)` vs `Typexp(a′)` and apply `sigmaReorder` when they differ.
    - Files: `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Sigma.hs`
    - Tests: add a case with mixed binders on `g` and `hg1i` (ordering mismatch) and assert that `applyInstantiation` matches `Typexp`.
    - _Requirements: 2.1, 2.2_
    - **Verification:** `cabal test`
  - [ ] 2.2 Document the reordering policy (based on <P) in `implementation_notes.md` with references to Def. 15.3.4.
    - _Requirements: 2.1_
    - **Verification:** doc review

- [ ] 3. Enforce translatable‑presolution invariants for Φ translation
  - [ ] 3.1 Add a validation pass that checks witness ops only target flexibly‑bound interior nodes and that computation contexts exist for each op.
    - Files: `src/MLF/Constraint/Presolution/Validation.hs` (or new module), `src/MLF/Elab/Phi.hs`
    - Tests: negative cases that previously “skipped” ops now fail with a clear error.
    - _Requirements: 3.2_
    - **Verification:** `cabal test`
  - [ ] 3.2 Replace silent skips of non‑interior ops with explicit errors (or prove unreachable and document).
    - Files: `src/MLF/Elab/Phi.hs`
    - Tests: confirm error surfaced for invalid witness.
    - _Requirements: 3.2_
    - **Verification:** `cabal test`

- [ ] 4. Align witness normalization with Fig. 15.3.4
  - [ ] 4.1 Ensure presolution can emit/normalize Raise operations on interior nodes where needed (or document deviation with rationale).
    - Files: `src/MLF/Constraint/Presolution/*`, `src/MLF/Constraint/Types.hs`
    - Tests: add witness normalization fixtures that include Raise/Weaken/Merge/Graft/RaiseMerge.
    - _Requirements: 3.3_
    - **Verification:** `cabal test`

