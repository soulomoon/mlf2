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

### BUG-2026-02-06-003
- Status: Open
- Priority: High
- Discovered: 2026-02-06
- Summary: Bounded aliasing (`b ⩾ a`) path that requires thesis Merge/RaiseMerge translation is not implemented end-to-end.
- Reproducer (test case):
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` case: `bounded aliasing (b ⩾ a) elaborates as ∀a. a -> a -> a (Merge/RaiseMerge path)`
- Reproducer (surface expression):
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (EVar "x"))) (STForall "a" Nothing (STForall "b" (Just (STVar "a")) (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))))`
- Expected:
  - Typechecks per thesis-aligned Merge/RaiseMerge witness path; equivalent to `∀a. a -> a -> a`.
- Actual:
  - Regression test expects success but currently fails with:
    - `PipelineTypeCheckError (TCLetTypeMismatch _ _)`
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
- Thesis impact:
  - Direct paper-faithfulness gap for bounded aliasing coercions requiring Merge/RaiseMerge composition.
- Detailed investigation (2026-02-07):
  - Validation command (direct reproducer):
    - `cabal exec -- runghc /tmp/check_bug003.hs` where `/tmp/check_bug003.hs` calls both `runPipelineElab` and `runPipelineElabChecked` on the reproducer expression.
  - Runtime result:
    - `runPipelineElab` and `runPipelineElabChecked` both fail with:
      - `Phase 7 (type checking): TCLetTypeMismatch ...`
    - Mismatch is between:
      - actual inferred let type: `∀a. ∀(t0 ⩾ ⊥ -> ⊥). t0 -> ⊥ -> t0`
      - expected annotation: `∀a. a -> a -> a`
  - Trace-backed phase findings:
    - Presolution edge-local unification does not emit Merge/RaiseMerge on the failing annotation path.
      - Debug trace shows: `shouldRecordRaiseMerge: binder=... bound=None`, so RaiseMerge is skipped.
      - Relevant code: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs:573`
    - Witness translation for the critical edge sees only `StepOmega (OpRaise ...)`, not `OpMerge`/`OpRaiseMerge`.
      - Relevant code paths:
        - witness construction/classification: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Witness.hs:135`
        - Ω interpretation of Raise/Merge/RaiseMerge: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:779`
    - Because Φ returns `InstId` on that edge, elaboration falls back to expansion-arg reconstruction (`reifyInst` fallback), which introduces `InstBot`/`⊥` arguments and yields the non-thesis type shape.
      - Relevant code: `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs:490`
  - Regression status:
    - `/Volumes/src/mlf4/test/ElaborationSpec.hs:2244`
  - Root-cause hypothesis:
    - The bounded aliasing coercion path is losing bounded-binder alias information before/while deciding chi_e witness ops, so the pipeline reaches a Raise-only + fallback-instantiation route instead of thesis Merge/RaiseMerge translation.
    - Secondary effect: fallback reconstruction is currently too permissive for this path and materializes `⊥` where alias-preserving instantiation is required.
  - Phase-order clarification:
    - This is **not** a pipeline-order bug. The repo still follows thesis order:
      - surface desugaring/coercion translation first;
      - then graphic-constraint presolution and normalized per-edge propagation witness construction.
    - The mismatch is semantic: current coercion alias lowering can erase explicit bounded-binder shape before edge-local unification computes `binderBounds`, while RaiseMerge emission is currently gated on that metadata.
    - Thesis RaiseMerge is propagation-structure-driven (`χe^p ⊑ χp` normalization), not syntax-presence-driven for alias bounds.
  - Rigid/non-translatable invariant status:
    - Existing strict invariants are still enforced and should remain unchanged:
      - merge-direction/translatability checks in witness normalization/validation stay active.
      - rigid-endpoint rejection behavior for merge-like ops remains covered by tests.
  - Acceptance-criteria gap (A7):
    - Not yet satisfied. Reproducer still fails in both unchecked and checked pipeline variants and does not produce `∀a. a -> a -> a`.
  - Proposed fix directions:
    - Replace `shouldRecordRaiseMerge` metadata-gating with live structural gating from the current constraint graph (canonical bounds + binding-tree ancestry + edge interior), so no alias-syntax survival metadata is required.
    - Guarantee bounded alias cases emit normalizable `OpMerge` or `OpRaise; OpMerge` (`OpRaiseMerge` after normalization) on the relevant edge.
    - Tighten/eliminate fallback-instantiation on this path when thesis witness translation should be authoritative.
    - Keep success assertions for both pipelines and drive them from red to green:
      - `runPipelineElab`
      - `runPipelineElabChecked`
    - Keep rigid/non-translatable Φ invariants unchanged and explicitly regression-test them alongside the bounded-aliasing success case.
- Locked decisions (2026-02-08):
  - Raw parser output remains part of long-term stable public API.
  - Normalization diagnostics remain structural-only for now.
  - External API split is explicit:
      - parser APIs may return raw syntax;
      - APIs that generate graphic constraints must take normalized syntax.

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
