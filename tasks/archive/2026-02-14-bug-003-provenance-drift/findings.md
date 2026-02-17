# Findings: BUG-2026-02-14-003

## 2026-02-14

- Reproduced baseline for this pass:
  - `BUG-003-V1/V2` failed with bottomized result type (`⊥ -> ⊥ -> ⊥ -> ⊥`).
  - `BUG-004` guard remained green.

- Probe and trace evidence showed:
  - Edge-0 Phi path no longer failed with non-binder after copy-map/binder alias handling, but the resulting let RHS still elaborated to bottomized arrows.
  - Edge-1 Phi then produced `InstInside (InstBot TBottom)` on the consumer edge, preserving the collapse.

- Root-cause finding for observed BUG-003 collapse:
  - In type checking of `ELet`, once `rhsTy` is accepted against the declared scheme `schTy`, the environment previously stored `rhsTy` (the bottomized concrete RHS type) rather than `schTy` (the declared interface).
  - For BUG-003, this leaked an over-specific/bottomized type for `c` into body checking and erased intended polymorphic behavior at the use site.

- Implemented fix:
  - `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs`: after `letSchemeAccepts rhsTy schTy`, store `schTy` in `termEnv` for the let-bound variable.

- Supporting hardening retained in this pass:
  - `/Volumes/src/mlf4/src/MLF/Elab/Inst.hs`: `InstApp` now accepts explicit non-`⊥` bounds when the argument matches structural bound shape (including `⊥` placeholders used by this path), while still rejecting explicit bound violations.
  - Existing strict non-binder rejection in Phi remains intact.

- Verification evidence:
  - `--match "BUG-003-V"` -> `2 examples, 0 failures`.
  - `--match "BUG-004"` -> `4 examples, 0 failures`.
  - `--match "Witness normalization invariants"` -> `10 examples, 0 failures`.
  - `--match "R-"` -> `19 examples, 0 failures`.
