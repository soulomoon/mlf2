# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

### BUG-2026-02-06-002
- Status: Open
- Priority: High
- Discovered: 2026-02-06
- Summary: Polymorphic factory (`make`) elaboration/generalization drifts; let scheme specializes while RHS remains polymorphic.
- Reproducer (surface expression):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4)))) (EApp (EVar "c1") (ELit (LBool True))))`
- Expected:
  - Pipeline elaborates and typechecks successfully (result should be `Int`).
  - `make` should have principal scheme `forall a b. a -> b -> a`.
  - `c1` (after applying `make` to `-4`) should have scheme `forall b. b -> Int`.
- Actual:
  - Current reproducer behavior on 2026-02-09 (latest checkpoint):
    - `Phase 7 (type checking): TCLetTypeMismatch (TForall "b" Nothing (TArrow TBottom (TBase Int))) (TForall "b" Nothing (TArrow (TVar "b") (TBase Int)))`.
  - Phase-2 replay on 2026-02-09 reconfirms a generalization-preprocessing ownership drift path:
    - `instCopyMap` includes `(25 -> NodeId 0)` and `p2CopyOverrides` mirrors it.
    - bind-parent ownership for node `25` transitions `GenNodeId 2 -> GenNodeId 4 -> GenNodeId 1` across Phase-3/Phase-4/final constraint assembly.
  - With presolution write-back pollution blocked, Phase 6 no longer fails at `OpGraft ... binder not found`; instead, edge-0 Φ translation still emits `InstBot ⊥` segments that force let-RHS shape toward `∀b. ⊥ -> Int`.
  - Focused regressions remain green in the current blocked state (`make const`, redirected let-use polymorphism, `BUG-2026-02-08-004` sentinel).
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs` (fixed anti-pollution gate now in place)
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Phase3.hs` (copy-parent grafting from `instCopyMap`)
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Phase4.hs` (scheme-interior owner realignment of inst-copy nodes)
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/Finalize.hs` (scheme anti-drift logic coupled with this path)
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs` (remaining edge-0 Φ translation fidelity issue)
- Thesis impact:
  - Valid let-polymorphic factory path remains non-elaboratable end-to-end under thesis-aligned generalization semantics.

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

### BUG-2026-02-08-006
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: Parser clean-break drift left legacy wrapper API (`parseEmlfExpr` / `parseEmlfType`) publicly exported.
- Reproducer:
  - `rg -n "\\bparseEmlfExpr\\b|\\bparseEmlfType\\b" /Volumes/src/mlf4/src/MLF/Frontend/Parse.hs /Volumes/src/mlf4/src-public/MLF/API.hs /Volumes/src/mlf4/test/FrontendParseSpec.hs`
- Expected:
  - Clean-break parser API exposes explicit staged entrypoints only (`parseRaw*`, `parseNorm*`), with no compatibility wrappers.
- Actual:
  - Legacy aliases remained exported and tests explicitly preserved them.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Frontend/Parse.hs`
  - `/Volumes/src/mlf4/src-public/MLF/API.hs`
  - `/Volumes/src/mlf4/test/FrontendParseSpec.hs`
- Fix:
  - Removed `parseEmlfExpr`/`parseEmlfType` exports and alias definitions from parser/API modules.
  - Updated frontend parse/pretty specs to use explicit raw parser entrypoints only.
- Regression tests:
  - `/Volumes/src/mlf4/test/FrontendParseSpec.hs`
  - `/Volumes/src/mlf4/test/FrontendPrettySpec.hs`
- Thesis impact:
  - Restores the locked staged-boundary migration decision (clean break, no compatibility wrappers), reducing ambiguity in raw-vs-normalized phase entry.

### BUG-2026-02-08-005
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: `normalizeType` had a reachable runtime crash in nested alias-bound normalization (`error "normalizeBound: unreachable"`).
- Reproducer (source type):
  - `STForall "x" (Just (STForall "b" (Just (STVar "a")) (STVar "b"))) (STVar "x")`
- Expected:
  - Total normalization through `Either NormalizationError ...` (typed `Left`), never process crash.
- Actual:
  - Runtime exception from `normalizeBound` `STVar` branch.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Frontend/Normalize.hs`
- Fix:
  - Added `NonStructuralBoundInStructContext SrcType` to `NormalizationError`.
  - Replaced the `STVar` crash path in `normalizeBound` with `Left (NonStructuralBoundInStructContext subtree)`.
- Regression test:
  - `/Volumes/src/mlf4/test/FrontendNormalizeSpec.hs` case `rejects nested alias bound that normalizes to a non-structural variable bound`
- Thesis impact:
  - Preserves deterministic, total frontend normalization at the raw→normalized boundary, matching the explicit `Either`-based failure model.

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
