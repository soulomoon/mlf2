# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

### BUG-2026-02-06-001
- Status: Open
- Priority: High
- Discovered: 2026-02-06
- Summary: Elaboration fails with `MissingNode` for a valid annotated-lambda application.
- Reproducer (surface expression):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EVar "id")))`
- Expected:
  - Pipeline elaborates and typechecks successfully (result should be `Int` under checked-authoritative behavior).
- Actual:
  - `Phase 6 (elaboration): MissingNode (NodeId {getNodeId = 38})`.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs`
- Thesis impact:
  - Breaks elaboration soundness for valid coercion/annotation flow.

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

### BUG-2026-02-06-003
- Status: Open
- Priority: High
- Discovered: 2026-02-06
- Summary: Bounded aliasing (`b ⩾ a`) path that requires thesis Merge/RaiseMerge translation is not implemented end-to-end.
- Reproducer (test case):
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` case: `bounded aliasing (b ⩾ a) remains a known Merge/RaiseMerge gap`
- Reproducer (surface expression):
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (EVar "x"))) (STForall "a" Nothing (STForall "b" (Just (STVar "a")) (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))))`
- Expected:
  - Typechecks per thesis-aligned Merge/RaiseMerge witness path; equivalent to `∀a. a -> a -> a`.
- Actual:
  - Expected-failure shape currently asserted in tests:
    - `PipelineTypeCheckError (TCLetTypeMismatch _ _)`
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
- Thesis impact:
  - Direct paper-faithfulness gap for bounded aliasing coercions requiring Merge/RaiseMerge composition.

## Resolved

- None yet.
