# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

### BUG-2026-02-08-001
- Status: Open
- Priority: High
- Discovered: 2026-02-08
- Summary: Task 7 verification gate is blocked by deterministic `shadow reify mismatch` failures in Phase 6 fallback generalization.
- Reproducer (test command):
  - `cabal test --test-show-details=direct --test-options="--match '/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/'"`
- Reproducer (surface expression from failing property):
  - `ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))`
- Expected:
  - `cabal build all && cabal test` passes, and solved-order/base-path shadow comparison accepts the elaborated identity shape.
- Actual:
  - Verification fails with Phase 6 `ValidationFailed ["shadow reify mismatch", ... "solved=t14 -> t14", "base=a -> a"]`, producing `506 examples, 4 failures`.
- Suspected area:
  - `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/src/MLF/Elab/Generalize.hs`
  - `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`
  - `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/src/MLF/Constraint/Presolution/Plan/Finalize.hs`
- Thesis impact:
  - Blocks the thesis-faithful elaboration path for let-polymorphic self-application (`id id`) due solved/base shadow disagreement in the fallback scheme route.

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

## Resolved

- None yet.
