# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

### BUG-2026-02-11-004
- Status: Open
- Priority: High
- Discovered: 2026-02-11
- Summary: Higher-arity bounded alias chains (`BUG-003-V1`, `BUG-003-V2`) fail in both pipelines with `OpGraft(non-binder)` `InstBot` Φ invariant errors.
- Minimal reproducers (surface expressions):
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (ELam "z" (EVar "x")))) (mkForalls [("a",Nothing),("b",Just (STVar "a")),("c",Just (STVar "b"))] (STArrow (STVar "a") (STArrow (STVar "b") (STArrow (STVar "c") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a"))))))`
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (ELam "z" (EVar "x")))) (mkForalls [("a",Nothing),("b",Just (STVar "a")),("c",Just (STVar "a"))] (STArrow (STVar "a") (STArrow (STVar "b") (STArrow (STVar "c") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a"))))))`
- Reproducer command:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V"'`
- Expected vs actual:
  - Expected: checked and unchecked both elaborate to `∀a. a -> a -> a -> a`.
  - Actual: both pipelines fail with `PhiInvariantError` containing `OpGraft(non-binder): InstBot expects ⊥`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`
- Thesis impact:
  - Leaves bounded-alias/raise-merge reconstruction incomplete for multi-binder, higher-arity aliasing chains.

## Resolved

### BUG-2026-02-11-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-11
- Summary: Thesis-hardening completion for extended polymorphic-factory variants (`BUG-002-V1..V4`) with strict Ω non-binder rejection guardrails.
- Minimal reproducers (surface expressions):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt 1))) (ELet "c2" (EApp (EVar "make") (ELit (LBool True))) (EApp (EVar "c1") (ELit (LBool False)))))`
  - `ELam "k" (ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (EVar "k")) (EApp (EVar "c1") (ELit (LBool True)))))`
- Final expected/actual:
  - Expected: V1..V3 elaborate to `Int`; V4 elaborates to `∀a. a -> a`; Ω rejects out-of-scheme/non-binder targets instead of fallback translation.
  - Actual (2026-02-11 verification): `BUG-002-V1..V4` pass in checked/unchecked pipelines; strict reject diagnostics for non-binder/out-of-scheme targets are preserved; full validation gate is green (`647 examples, 0 failures`).
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-002-V1..V4`, `BUG-003-V1`, `BUG-003-V2`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V"'` (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "out-of-scheme target"'` (`2 examples, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`647 examples, 0 failures`)
- Thesis impact:
  - Restores thesis-aligned Φ/Ω translatability behavior for polymorphic-factory paths and removes non-binder fallback translation in favor of strict rejection.

### BUG-2026-02-11-003
- Status: Resolved (Thesis-exact)
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-12
- Summary: Nested annotation variants for BUG-004 (`V2`, `V4`) now pass under strict-only elaboration/typechecking with no compatibility fallback paths.
- Minimal reproducers (surface expressions):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EAnn (EVar "id") (STArrow (STBase "Int") (STBase "Int")))))`
  - `EApp (ELamAnn "seed" (STBase "Int") (ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (EVar "seed"))) (EApp (EVar "use") (EVar "id"))))) (ELit (LInt 1))`
- Root cause:
  - V2: call-site annotation elaborated to a bounded-forall term and then received an additional inferred `InstApp`; strict `InstBot` rejects `InstApp` on already bounded foralls (`InstBot expects TBottom`).
  - V4: `generalizeAtNode` wrapped monomorphic annotations in trivially bounded foralls (`∀(a:Int→Int).a`), causing downstream `InstApp` to fail on non-⊥ bounds.
- Fix:
  - Omega.hs: tightened bare `InstBot` production to require `TBottom` input (changed `ty == TBottom || alphaEqType ty argTy` to `alphaEqType ty TBottom`).
  - Elaborate.hs ALamF: collapse trivially bounded foralls `∀(name:B).name` to bound type `B` for annotated lambda parameters.
  - Elaborate.hs AAppF: normalize inferred argument instantiation to `InstElim` when argument is already `∀(⩾ τ)` (annotation updated the bound), and `InstId` when argument is already monomorphic.
  - PhiReorder: restrict reorder identity to scheme-owned binder positions only.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-004-V1..V4`)
  - `/Volumes/src/mlf4/test/TypeCheckSpec.hs` (strict InstBot regressions: `InstInside(InstBot)` accept/reject, bare `InstBot` reject)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'` (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 7 typecheck"'` (`13 examples, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`652 examples, 0 failures`)
- Thesis impact:
  - Strict `InstBot` checker semantics are unchanged; only instantiation *production* was corrected. The checker still rejects `InstBot` on any non-⊥ input. This is thesis-exact: the paper's `⊥ ← τ` rule requires the input to be `⊥`.

### BUG-2026-02-06-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-02-11
- Summary: Polymorphic factory (`make`) elaboration/generalization drift no longer reproduces; checked and unchecked pipelines now return `Int`.
- Reproducer (surface expression):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4)))) (EApp (EVar "c1") (ELit (LBool True))))`
- Final expected/actual:
  - Expected: Pipeline elaborates and typechecks successfully (result `Int`) with polymorphic `make`/`c1` behavior.
  - Actual (2026-02-11 verification): both unchecked and checked pipelines return `Int`; targeted regression suite passes and full gate is green (`633 examples, 0 failures`).
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'` (`10 examples, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`633 examples, 0 failures`)
- Thesis impact:
  - Restores thesis-aligned let-polymorphic factory behavior for the guarded `make` path.

### BUG-2026-02-08-004
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-11
- Summary: Nested let + annotated-lambda application no longer fails with `TCLetTypeMismatch`; checked and unchecked pipelines now typecheck to `Int`.
- Reproducer (surface expression):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EVar "id")))`
- Final expected/actual:
  - Expected: Pipeline elaborates and typechecks successfully (result `Int`).
  - Actual (2026-02-11 verification): both unchecked and checked pipelines return `Int`; full gate is green (`633 examples, 0 failures`).
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'` (`1 example, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`633 examples, 0 failures`)
- Thesis impact:
  - Restores checked-authoritative behavior for the guarded nested let + annotated-lambda path.

### BUG-2026-02-11-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-11
- Summary: Phase 3 paper-shaped residual-edge wrapping regressed elaboration/typechecking due synthesized-wrapper misclassification and strict Phi reorder keying.
- Reproducer (test command):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'`
- Expected:
  - Gate suite remains equivalent to the Phase 2 baseline (all 7 gate cases pass) under strict paper-shaped wrapping.
- Actual (before fix):
  - Gate suite failed with `SchemeFreeVars`, `TCTypeAbsVarInScope`, then `PhiInvariantError "PhiReorder: missing order key ..."` depending on intermediate fixes.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Normalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Fix:
  - Reserved negative `ExpVarId` space for synthesized wrappers in normalization.
  - Switched synthesized-wrapper dispatch to `ExpVarId < 0` (instead of TyExp-body-shape heuristic), preserving real expansion semantics for frontend TyExp edges.
  - Added Phi reorder fallback to full order-key map when narrowed binder-key map is incomplete.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`describe "Phase 3 atomic wrapping equivalence gates"`)
  - Validation gate: `cabal build all && cabal test`
- Thesis impact:
  - Restores strict paper-shaped residual-edge representation without changing elaboration outcomes on the guarded thesis target matrix.

### BUG-2026-02-10-001
- Status: Resolved
- Priority: Medium
- Discovered: 2026-02-10
- Resolved: 2026-02-10
- Summary: Fig. 15.3.4 witness normalization/emission closure lacked an explicit 15-row matrix contract with row-id evidence in tests/docs.
- Reproducer (test command):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`
- Expected:
  - Explicitly named and executable 15-row matrix coverage for `Graft`/`Weaken`/`Merge`/`Raise`/`RaiseMerge` valid/invalid/norm cases, with deterministic outcomes and green closure gate.
- Actual (before fix):
  - Fig. 15.3.4 witness coverage existed but was not fully represented as a row-ID closure matrix (`R-*-*-NN`) and Task 4 remained marked partial/open.
- Suspected area:
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`
  - `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs`
  - `/Volumes/src/mlf4/.kiro/specs/paper-faithfulness-remaining-deltas/*`
- Fix:
  - Added/renamed row-labeled tests covering all 15 matrix rows (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`).
  - Added missing Raise VALID/NORM row assertions with strict expected outcomes.
  - Updated `.kiro` requirements/tasks status and supporting docs to reflect closure evidence.
- Regression tests:
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`
  - `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs`
  - Matrix gate: `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`
  - Full gate: `cabal build all && cabal test`
- Thesis impact:
  - Converts Fig. 15.3.4 witness alignment from partial coverage to explicit, auditable closure contract evidence without introducing behavior deviations.

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
  - The prior type-checking follow-up for the same surface program is now resolved as `BUG-2026-02-08-004` (2026-02-11).

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
