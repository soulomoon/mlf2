# Implementation Plan

## Closure Note (2026-02-17)
- This spec’s semantic paper-faithfulness tasks are complete.
- Keep this artifact as historical closure evidence for A4 doc sync.
- Remaining open work is outside this spec’s semantic scope (proof formalization, broader regression breadth, API/harness cleanup), tracked in `/Volumes/src/mlf4/TODO.md`.

- [x] 1. Add constructor types to xMLF AST
  - [x] 1.1 Extend `Ty`/`TyIF` with a constructor type (e.g., `TCon`) and update folds/pretty printers.
    - Files: `src/MLF/Types/Elab.hs`, `src/MLF/Elab/Types.hs`
    - Tests: optional follow-up — add pretty/round‑trip cases in `test/TypeCheckSpec.hs`
    - _Requirements: 1.1_
    - **Verification:** `cabal test`
  - [x] 1.2 Update instantiation/typechecking to allow constructor types in bounds and terms.
    - Files: `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `src/MLF/Elab/Reduce.hs`
    - Tests: optional follow-up — add instantiation/typechecking cases for `Cσ`.
    - _Requirements: 1.1, 1.2_
    - **Verification:** `cabal test`

- [x] 2. Integrate quantifier reordering ϕR in elaboration
  - [x] 2.1 Compute `Typ(a′)` vs `Typexp(a′)` and apply `sigmaReorder` when they differ.
    - Files: `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Phi/Omega.hs`, `src/MLF/Elab/Sigma.hs`
    - Tests: add a case with mixed binders on `g` and `hg1i` (ordering mismatch) and assert that `applyInstantiation` matches `Typexp`.
    - Current status: covered by `test/ElaborationSpec.hs` (“applies Σ reordering even without Raise when Typ/Typexp differ”).
    - _Requirements: 2.1, 2.2_
    - **Verification:** `cabal test`
  - [x] 2.2 Document the reordering policy (based on <P) in `implementation_notes.md` with references to Def. 15.3.4.
    - _Requirements: 2.1_
    - **Verification:** doc review

- [x] 3. Enforce translatable‑presolution invariants for Φ translation
  - [x] 3.1 Add a validation pass that checks witness ops only target flexibly‑bound interior nodes and that computation contexts exist for each op.
    - Files: `src/MLF/Constraint/Presolution/Validation.hs` (or new module), `src/MLF/Elab/Phi/Omega.hs`
    - Tests: negative cases that previously “skipped” ops now fail with a clear error.
    - Current status: normalization/validation enforces interior/order invariants; Ω translation requires explicit non-spine contexts and reports explicit Φ errors.
    - _Requirements: 3.2_
    - **Verification:** `cabal test`
  - [x] 3.2 Replace silent skips of non‑interior ops with explicit errors (or prove unreachable and document).
    - Files: `src/MLF/Elab/Phi/Omega.hs`
    - Tests: confirm error surfaced for invalid witness.
    - Current status: `OpRaise` non-spine fallback branches were removed; root/non-binder graft handling is explicit; non-translatable paths fail via `PhiTranslatabilityError`.
    - Note: US-004 κσ deviation is resolved (2026-02-10): `test/ElaborationSpec.hs` and `test/PipelineSpec.hs` now assert the thesis-expected `Int` behavior for annotated-lambda polymorphic-argument flow.
    - _Requirements: 3.2_
    - **Verification:** `cabal test`

- [x] 4. Align witness normalization with Fig. 15.3.4
  - [x] 4.1 Ensure presolution can emit/normalize Raise operations on interior nodes where needed (or document deviation with rationale).
    - Files: `src/MLF/Constraint/Presolution/*`, `src/MLF/Constraint/Types/Witness.hs`
    - Tests: add witness normalization fixtures that include Raise/Weaken/Merge/Graft/RaiseMerge.
    - Current status (2026-02-10): strict transitive-flex guard for `OpRaise` is implemented in normalized witness validation; Fig. 15.3.4 15-row matrix rows `R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15` are explicitly named across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`; matrix closure gate and full gate are green (`cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`, `cabal build all && cabal test`).
    - _Requirements: 3.3_
    - **Verification:** `cabal test`
