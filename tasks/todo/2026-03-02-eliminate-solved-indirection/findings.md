# Findings: Eliminate Solved Indirection

## Key Discoveries
- `Solved` construction in production pipeline is concentrated in `src/MLF/Elab/Run/Pipeline.hs` via `buildSolvedFromPresolutionSnapshot` and `runPipelineElabWithSolvedBuilder`.
- Secondary replay/rebuild usage exists in pipeline (`Solved.rebuildWithConstraint`) and result-type fallback (`Solved.rebuildWithNodes`).
- Phi/Omega cluster is the densest consumer set:
  - heavy usage in `src/MLF/Elab/Phi/Omega.hs`
  - supporting usage in `Context.hs`, `Translate.hs`
  - type-threaded references in `Env.hs`, `IdentityBridge.hs`, `TestOnly.hs`
- Reify/Generalize cluster has fewer but semantically sensitive uses:
  - `Solved.canonicalConstraint` and `Solved.canonical` in `src/MLF/Elab/Run/Generalize.hs`
  - broad query usage in `src/MLF/Reify/Core.hs`
- Presolution planning closure currently captures `Solved` through `PresolutionPlanBuilder` type in `src/MLF/Constraint/Presolution/Base.hs` and `buildGeneralizePlans` in `src/MLF/Constraint/Presolution/Plan.hs`.
- Concrete closure/call chain for planner:
  - `prPlanBuilder` field in `PresolutionResult` (`src/MLF/Constraint/Presolution/Base.hs`)
  - captured in driver (`src/MLF/Constraint/Presolution/Driver.hs`)
  - consumed in pipeline (`src/MLF/Elab/Run/Pipeline.hs`)
  - unwrapped in generalize runner (`src/MLF/Elab/Run/Generalize.hs`)
- `Solved.canonicalConstraint` is explicitly consumed in:
  - `src/MLF/Constraint/Presolution/Plan.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
  - `src/MLF/Reify/Core.hs` (as reify node source)

## Migration Risks
- `prPlanBuilder` signature change is cross-cutting and should happen in one coordinated phase.
- `Omega.hs` refactor carries highest behavioral regression risk due operation translation complexity.
- `canonicalConstraint` assumptions in Generalize/Reify must be made explicit as `pvConstraint + pvCanonical` derivations.
- Production path still contains `Solved.rebuildWithNodes` in result-type fallback, which can silently reintroduce solved reconstruction unless removed in the migration.

## Team D Session Findings (Wave 1 Tasks 3 + 6)
- `ElabEnv` still exposes `eeSolved :: Solved` in `src/MLF/Elab/Elaborate.hs`.
- Pipeline still routes through solved-builder indirection:
  - `runPipelineElabWithSolvedBuilder`
  - `type SolvedBuilder = ...`
  - `buildSolvedFromPresolutionSnapshot`
- Result-type context still stores solved handle directly:
  - `rtcSolved :: Solved` in `src/MLF/Elab/Run/ResultType/Types.hs`
  - downstream reads in `ResultType.hs`, `ResultType/Ann.hs`, `ResultType/Fallback.hs`.
- Existing guard coverage partially overlaps:
  - already guards `runPipelineElabWithSolvedBuilder` removal
  - does not yet guard `buildSolvedFromPresolutionSnapshot` and `eePresolutionView`/`rtcSolved` boundary intent.

## Team D Session Outcomes (Wave 1 Tasks 3 + 6)
- `ElabEnv` now stores `eePresolutionView :: PresolutionView`; `eeSolved` field removed.
- `runPipelineElabWithSolvedBuilder`, `SolvedBuilder`, and `buildSolvedFromPresolutionSnapshot` were removed from `src/MLF/Elab/Run/Pipeline.hs`.
- Pipeline now converts solved artifacts to boundary view via `solvedToPresolutionView` and threads that view into:
  - `ElabEnv.eePresolutionView`
  - `ResultTypeInputs.rtcPresolutionView`
- Result-type context no longer stores `rtcSolved`; it reconstructs a local solved handle through `rtcSolveLike` when needed for legacy solved-query consumers.
- Guard tests now enforce all three Team D migration contracts:
  - `ElabEnv uses PresolutionView boundary`
  - `pipeline no longer defines buildSolvedFromPresolutionSnapshot`
  - `result-type context no longer stores rtcSolved`

## Team C Session Findings (Wave 1 Task 5)
- `src/MLF/Elab/Run/Generalize.hs` still builds `GeneralizeEnv` canonical inputs directly from `Solved.canonicalConstraint` and `Solved.canonical`.
- `src/MLF/Elab/Run/Generalize/Constraint.hs` still derives instantiation-copy canonicalization directly from `Solved.canonical`.
- `src/MLF/Reify/Core.hs` currently anchors reify traversal nodes to `Solved.canonicalConstraint`; preserving behavior while switching canonical/constraint reads to `PresolutionView` is the low-risk migration path.

## Team C Session Outcomes (Wave 1 Task 5)
- `Generalize` canonical constraint setup now depends explicitly on `PresolutionView` (`pvCanonicalConstraint` + `pvCanonical`) instead of direct solved queries.
- `instantiationCopyNodes` now canonicalizes via `PresolutionView`, and pipeline now threads `presolutionViewClean` into generalization preparation.
- Reify-side direct `PresolutionView` imports are currently blocked by an internal module cycle (`Presolution.Base -> Plan -> ReifyPlan -> Reify.TypeOps`), so this session kept Reify APIs behavior-stable and limited explicit view propagation to the Generalize boundary.

## Validation Baseline
- Worktree baseline command succeeded:
  - `cabal build all && cabal test`
  - `895 examples, 0 failures`
- Wave 2 closeout gate succeeded:
  - `cabal build all && cabal test`
  - `905 examples, 0 failures`

## Team B Session Findings (Wave 1 Task 4)
- `src/MLF/Elab/Phi/Context.hs` can be fully migrated to view-backed queries; it no longer needs `Solved`-specific APIs when using `Order.orderKeysFromConstraintWith`.
- `src/MLF/Elab/Phi/Omega.hs` can consume all graph/binding lookups through `PresolutionView` query fields when reify operations are injected as closures from `Translate`.
- `src/MLF/Elab/Phi/IdentityBridge.hs` does not require `Solved`; storing/reading `PresolutionView` is sufficient for bridge ranking/reconciliation.
- `src/MLF/Elab/Phi/Translate.hs` still needs `Solved` at the API edge for compatibility (`GeneralizeAtWith` callback + existing callers), but core operational queries are now view-backed and threaded through `OmegaContext`.
- The requested OR-pattern verification commands still match 0 examples in this suite; concrete per-slice matches provide meaningful execution coverage.

## Team E Session Findings (Wave 2 Tasks 7-9)
- `PresolutionPlanBuilder` type migration to `PresolutionView` required a cycle break between `Presolution.Base` and `Presolution.View`; generalizing `fromPresolutionResult` to `PresolutionSnapshot a => a -> PresolutionView` removed the cycle and preserved existing call sites.
- Planner/generalize call chain is now view-first:
  - `PresolutionPlanBuilder.ppbBuildGeneralizePlans :: PresolutionView -> ...`
  - `buildGeneralizePlans` now consumes `PresolutionView` canonical data directly.
  - `generalizeAtWithBuilder` now derives a `PresolutionView` adapter from the runtime `Solved` handle before invoking the plan builder.
- `Solved.fromPresolutionResult` was production-only surface with no production call sites; removal is behavior-neutral for runtime path (pipeline already uses `fromPreRewriteState`).
- First compatibility reconstruction attempt inside planner (`Solved.fromPreRewriteState` from view snapshot) caused binding-tree regressions in `Paper alignment baselines`; replacing with `Solved.mkTestSolved` over canonical constraint plus live-node canonical-map sanitization restored behavior.
- Full-tree solved-import ban in elaboration internals is not currently compatible with architecture (internal result-type/generalize code still uses solved compatibility APIs); implemented nearest enforceable hygiene guard on public/entrypoint elaboration modules (`MLF.Elab.Run`, `MLF.Elab.Pipeline`, `MLF.API`, `MLF.Pipeline`).

## Team E Session Outcomes (Wave 2 Tasks 7-9)
- Guard coverage added and green:
  - `PresolutionPlanBuilder closes over PresolutionView, not Solved`
  - `Solved production-only builders are absent`
  - `production src tree has no MLF.Constraint.Solved imports in elaboration path` (entrypoint scope)
- `MLF.Constraint.Solved` reduced by removing `fromPresolutionResult` export/implementation; `Constraint.SolvedSpec` parity guard now uses `fromPreRewriteState (snapshotUnionFind/snapshotConstraint)`.
- Docs/task tracking updated for Wave 2 completion and compatibility-boundary rationale.
