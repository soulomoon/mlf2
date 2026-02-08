# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

### BUG-2026-02-06-002
- Status: Open
- Priority: High
- Discovered: 2026-02-06
- Summary: Φ translation fails with binder-spine mismatch on polymorphic factory use.
- Reproducer (surface expression):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4)))) (EApp (EVar "c1") (ELit (LBool True))))`
- Expected:
  - Pipeline elaborates and typechecks successfully (result should be `Int`).
- Actual:
  - `Phase 6 (elaboration): PhiTranslatabilityError ["OpGraft: binder not found in quantifier spine", ...]`.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Thesis impact:
  - Indicates witness-to-Φ translation is still incomplete for a valid let-polymorphic application path.

### BUG-2026-02-08-004
- Status: Open
- Priority: High
- Discovered: 2026-02-08
- Summary: Nested let + annotated-lambda application now reaches Phase 7 but fails with `TCLetTypeMismatch`.
- Reproducer (surface expression):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EVar "id")))`
- Expected:
  - Pipeline elaborates and typechecks successfully (result `Int`).
- Actual:
  - `Phase 7 (type checking): TCLetTypeMismatch (TForall "a" Nothing (TArrow (TArrow Int Int) Int)) (TForall "a" Nothing (TArrow (TVar "a") Int))`.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/Target/GammaPlan.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`
- Thesis impact:
  - Valid coercion/annotation flow still does not typecheck under checked-authoritative semantics.

## Resolved

### BUG-2026-02-06-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-02-08
- Summary: Eliminated Phase 6 `MissingNode` crash on nested let + annotated-lambda application.
- Root cause:
  - `reifyWithGaBase` accepted `solvedToBasePref` entries whose base node did not exist in `gaBaseConstraint`, then called base-constraint reification on that stale node id.
- Fix:
  - Added a base-node existence guard in `/Volumes/src/mlf4/src/MLF/Elab/Generalize.hs` so base-constraint reification is only used when that node is present; otherwise elaboration falls back to solved-order reification.
- Regression test:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`nested let + annotated lambda application does not crash in Phase 6 (BUG-2026-02-06-001)`).
- Follow-up:
  - Remaining type-checking failure for the same surface program is tracked as `BUG-2026-02-08-004`.

### BUG-2026-02-06-003
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-02-08
- Summary: Bounded aliasing (`b ⩾ a`) Merge/RaiseMerge path now elaborates end-to-end.
- Fix:
  - RaiseMerge gating now uses live structural graph queries in `shouldRecordRaiseMerge` (canonical bound lookup + ancestry/interior + elimination state), with no precomputed binder-bound snapshots.
  - Edge-local elimination persists binder substitution targets before elimination, preserving witness inputs required for thesis-aligned Φ translation.
  - Witness normalization interior widening is restricted to multi-binder edge traces so required alias-path evidence is preserved without broad translatability regressions.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` case: `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` section: `Witness normalization invariants (US-010 regression)`
- Thesis impact:
  - Closes the bounded-alias paper-faithfulness gap by restoring the thesis-aligned `∀a. a -> a -> a` baseline in both checked and unchecked elaboration pipelines.

### BUG-2026-02-08-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: Phase 6 fallback generalization shadow comparison rejected semantically equivalent solved/base reifications when names diverged (`t14 -> t14` vs `a -> a`).
- Root cause:
  - `shadowCompareTypes` in `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/src/MLF/Elab/Generalize.hs` only used `alphaEqType`, which permits bound-variable renaming but rejects free-name-only renaming from solved/base reify paths.
  - Fallback comparison in `generalizeAt:fallbackSchemeType` compared solved reification (`rpSubst`) against base-path shadow reification (`rpSubstBaseByKey`), and those maps can assign different names to the same shape.
- Fix:
  - Added a bijective variable-renaming comparator (`alphaEqTypeModuloVarRenaming`) and used it in `shadowCompareTypes`.
  - Added regression coverage in `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/test/GeneralizeSpec.hs` for:
    - accepting same-structure free-name divergence
    - rejecting inconsistent (non-bijective) reuse
- Reproducer now passes:
  - `cabal test --test-show-details=direct --test-options='--match=redirected --skip=instantiation'`
- Regression checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="US-004"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="id y should have type"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="mapped-base elaboration"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="redirected let-use"'`
  - `cabal build all && cabal test`
  - Task 7 closure gate rerun (2026-02-08, local `+0800`): `cabal build all && cabal test` passed 5/5 consecutively
    - `1/5` `2026-02-08T16:17:43+0800` -> `2026-02-08T16:17:46+0800`
    - `2/5` `2026-02-08T16:17:46+0800` -> `2026-02-08T16:17:49+0800`
    - `3/5` `2026-02-08T16:17:49+0800` -> `2026-02-08T16:17:52+0800`
    - `4/5` `2026-02-08T16:17:52+0800` -> `2026-02-08T16:17:55+0800`
    - `5/5` `2026-02-08T16:17:55+0800` -> `2026-02-08T16:17:58+0800`
- Thesis impact:
  - Restores thesis-faithful elaboration for redirected let-use identity without weakening mismatch detection for structurally different types.
