# Findings: Solved Module Audit

## Module Inventory

### Boundary Summary
- `src-public/` and `app/` contain no direct `MLF.Constraint.Solved` imports.
- Runtime solved coupling is concentrated in `src/MLF/Elab/*`, `src/MLF/Reify/*`, `src/MLF/Constraint/Presolution/Plan/*`, and `src/MLF/Util/Order.hs`.
- `MLF.Constraint.Solved` remains a shared runtime + compatibility utility surface; tests rely heavily on `mkTestSolved`/`fromPreRewriteState` helper paths.

### Production-Runtime Touchpoints (selected)
- `src/MLF/Elab/Elaborate.hs`
- `src/MLF/Elab/Phi/Env.hs`
- `src/MLF/Elab/Phi/Translate.hs`
- `src/MLF/Elab/Generalize.hs`
- `src/MLF/Elab/Run/{Pipeline,Generalize,ResultType,ResultType/Ann,ResultType/Fallback,Scope,TypeOps}.hs`
- `src/MLF/Reify/{Core,TypeOps}.hs`
- `src/MLF/Util/Order.hs`

### Compat/Test-Weighted Touchpoints (selected)
- `src/MLF/Constraint/Presolution/Plan.hs` (`Solved.mkTestSolved` adapter path)
- `src/MLF/Constraint/Presolution/Plan/{Context,BinderPlan/Types,ReifyPlan}.hs`
- `src/MLF/Elab/Legacy.hs`
- `src/MLF/Elab/Phi/TestOnly.hs` (explicit test-only module in `src/`)
- `test/Constraint/SolvedSpec.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/Parity/FrozenArtifacts.hs`, `test/SpecUtil.hs`

## Production vs Compat/Test Boundary Notes

### Confirmed Non-Violations
- No solved imports in public/executable boundaries:
  - `src-public/MLF/{API,Pipeline,XMLF}.hs`
  - `src-public/MyLib.hs`
  - `app/Main.hs`

### Active Coupling Patterns
- Solved-to-`PresolutionView` adapter logic is duplicated in runtime modules:
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Phi/Translate.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
  - `src/MLF/Elab/Run/Pipeline.hs`
- Production code still calls compatibility-labeled helper construction (`Solved.mkTestSolved`) in planner/reify pathways.
- Result-type flow rematerializes solved values from `PresolutionView` (`rtcSolveLike`) rather than threading one authoritative handle.

## Production Call Chains with Solved-Boundary Assumptions
1. `MLF.Pipeline.runPipelineElab*` -> `MLF.Elab.Run.Pipeline.runPipelineElabWith` -> `Solved.fromPreRewriteState` -> `setSolvedConstraint` / canonical validation.
- Assumptions: reconstruction stability and canonical replay correctness.

2. `runPipelineElabWith` -> `generalizeAtWithBuilder` -> `PresolutionPlanBuilder` -> `buildGeneralizePlans` -> solved reconstruction from view.
- Assumptions: solved/view round-tripping is lossless.

3. `buildGeneralizePlans` -> `resolveContext` -> `gaSolvedToBase` + fallback scope resolution -> `Solved.rebuildWithConstraint`.
- Assumptions: fallback resolution is semantically safe.

4. `buildGeneralizePlans` -> binder plan ordering -> `Util.Order.orderKeysFromConstraintWith`.
- Assumptions: solved-order fallback when base order keys are absent is behavior-neutral.

5. `applyGeneralizePlan` -> `Reify.*OnConstraint` -> `Reify.Core.reifyWith` (`mkTestSolved` path).
- Assumptions: pseudo-solved conversion preserves runtime semantics.

6. `runPipelineElabWith` -> result-type primary/fallback (`rtcSolveLike`) -> localized solved rebuild.
- Assumptions: local rebuild/rewrite patches remain equivalent to authoritative solved state.

7. `runPipelineElabWith` -> `elaborateWithEnv`/`reifyInst` -> `phiFromEdgeWitnessWithTrace`.
- Assumptions: solved/base mapping + replay fallback chain remains complete on edge cases.

## Thesis-Faithfulness Touchpoints

### Mapped Obligations / Claims
- Solver correctness (`CLM-SOLVER-CORRECTNESS`): `src/MLF/Constraint/{Solve,Presolution/Driver,Solved}.hs` with `test/SolveSpec.hs`, `test/Presolution/EnforcementSpec.hs`.
- Translatable presolution (`CLM-TRANSLATABLE-PRESOLUTION`): `src/MLF/Constraint/Presolution/{Validation,Driver}.hs` with `test/TranslatablePresolutionSpec.hs`.
- Witness translation / Phi correctness (`CLM-WITNESS-TRANSLATION`, `CLM-PHI-CORRECTNESS`): `src/MLF/Elab/Phi/{Translate,Omega,IdentityBridge}.hs` with `test/Presolution/WitnessSpec.hs`, `test/Phi/*`, `test/ElaborationSpec.hs`.
- Sigma reorder (`CLM-SIGMA-REORDER`): `src/MLF/Elab/{Sigma,Elaborate}.hs` with `test/ElaborationSpec.hs`.
- Elaboration pipeline correctness (`CLM-ELABORATION-CORRECTNESS`): `src/MLF/Elab/Run/{Pipeline,ResultType/Types}.hs` + `src/MLF/Elab/Elaborate.hs` with `test/PipelineSpec.hs`.

### Candidate Mismatch / Deviation Risks
- Medium / High confidence: documentation naming drift still references solved-centric handles where runtime now prefers `PresolutionView` boundaries.
- Medium / Medium confidence: `MLF.Constraint.Solved` export breadth mixes read-style narrative with rebuild/prune helpers.
- Low / Medium confidence: claim/obligation anchors sometimes point at facades while enforcement lives in runtime modules.
- Low / Medium confidence: O15 empty-sequence translation appears under-tested in isolation from coupled reorder paths.

## Migration Risks
- Behavior drift risk if `gaSolvedToBase` fallback behavior is changed without replay-focused regression expansion.
- Hidden semantic coupling risk from `mkTestSolved` in runtime paths.
- Boundary confusion risk from mixed solved/view adapter implementations.
- Auditability risk if docs/claims anchors are not aligned to runtime modules.

## Prioritized Migration Actions and Guard Tests
1. Introduce a single shared `Solved -> PresolutionView` adapter helper and remove duplicate local constructions.
2. Replace runtime `mkTestSolved` usages with a production-safe constructor boundary (or view-native APIs).
3. Tighten mapping types for solved/base origin and remove silent default fallbacks in context resolution.
4. Split binder-order logic into explicit base-domain vs solved-domain paths with assertions for fallback entry.
5. Add guard tests:
- solved canonical/original agreement and canonicalized bind-parent projection invariants,
- explicit boundary guard for new runtime `rebuildWith*` call sites,
- isolated O15 `Trχ(ε)=ε` coverage,
- result-type primary vs fallback equivalence on replay-heavy scenarios.

## Recommended Ownership Split
- Team A (Pipeline + ResultType): adapter dedup, result-type solved rematerialization removal.
- Team B (Presolution Plan + Reify): remove `mkTestSolved` runtime dependency and tighten mapping fallbacks.
- Team C (Docs + Claims + Tests): anchor realignment, naming cleanup, solved-boundary guard-test additions.
