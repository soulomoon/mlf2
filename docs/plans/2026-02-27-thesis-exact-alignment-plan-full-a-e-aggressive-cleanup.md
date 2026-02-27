# Thesis-Exact Alignment Plan (Full A-E, Aggressive Cleanup)

## Summary

Implement thesis-exact behavior end-to-end by making `SolveConstraint` ordering explicit, keeping graph transformation ownership in presolution/solve, making `Solved` a strict projection layer, and enforcing witness-domain-first Phi translation.

Primary thesis anchors:
1. Solve ordering: `SolveConstraint` step order ([these-finale-english.txt](/Volumes/src/mlf4/papers/these-finale-english.txt):12240-12255)
2. Propagation semantics: ([these-finale-english.txt](/Volumes/src/mlf4/papers/these-finale-english.txt):10776-10865)
3. Phi from propagation witness/`χp`: ([these-finale-english.txt](/Volumes/src/mlf4/papers/these-finale-english.txt):17794-18040)

## Current-State Grounding

1. Presolution already does edge-ordered processing + closure drains ([EdgeProcessing.hs](/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs):53-58), but closure currently rewrites constraint with UF before each drain ([EdgeProcessing.hs](/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs):73-76).
2. Pipeline already builds solved view from presolution ([Pipeline.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs):95-103), and has a projection-first variant ([Pipeline.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs):228-253).
3. Phi identity resolution improved with witness-domain-first ranking note ([IdentityBridge.hs](/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs):144-196) and trace-first weaken recovery ([Omega.hs](/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs):1260-1311).
4. Remaining divergence hotspots: canonical-domain reads in reify/fallback/generalize ([Reify/Core.hs](/Volumes/src/mlf4/src/MLF/Reify/Core.hs):94-137, [Fallback.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs):488-523).

## Public API / Interface Changes

1. Add seeded closure API:
- `runUnifyClosureWithSeed :: TraceConfig -> IntMap NodeId -> Constraint -> Either SolveError UnifyClosureResult`
- Location: [Unify/Closure.hs](/Volumes/src/mlf4/src/MLF/Constraint/Unify/Closure.hs)
2. Tighten `Solved` public surface (aggressive cleanup):
- Remove canonical-domain query exports from public API after migration:
  - `canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`
- Keep only projection/query API needed by consumers:
  - `canonical`, `lookupNode`, `allNodes`, `lookupBindParent`, `bindParents`, `instEdges`, `genNodes`, `classMembers`, `originalConstraint`
- Location: [Solved.hs](/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs)
3. Remove transitional pipeline entrypoint after cutover:
- Remove `runPipelineElabProjectionFirst` once default path is projection-first and validated
- Location: [Pipeline.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs)

## Implementation Plan

## Phase A - Thesis-Exact Ordering (SolveConstraint Shape)

1. Replace pre-drain rewrite loop with seeded closure.
- Change [EdgeProcessing.hs](/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs):
  - Keep one initial closure drain (thesis step 2 equivalent).
  - For each sorted inst edge: `processInstEdge` then one closure drain (thesis step 3a/3b pair).
  - Remove `rewriteConstraintWithUF` from `drainPendingUnifyClosure`.
  - Call new `runUnifyClosureWithSeed traceCfg psUnionFind psConstraint`.
2. Implement seeded closure in [Unify/Closure.hs](/Volumes/src/mlf4/src/MLF/Constraint/Unify/Closure.hs):
- Start with provided UF seed instead of empty UF.
- Maintain same harmonization behavior (`batchHarmonize`).
- Return drained `cUnifyEdges = []` and updated UF.
3. Add hard boundary assertions in presolution loop:
- Before processing an edge, assert no pending unify edges.
- After post-edge closure, assert no pending unify edges.
- Fail with edge id/context.

Exit criteria:
1. Presolution loop trace matches thesis step order exactly.
2. No per-drain graph rewrite occurs outside final presolution rewrite/materialization step.
3. All tests pass.

## Phase B - Presolution Artifact Contract (Thesis `χp`)

1. Enforce contract on `PresolutionResult` at producer boundary in [Driver.hs](/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs):
- `cUnifyEdges == []`
- `cInstEdges == []` after rewrite/materialization
- no `TyExp` nodes in `prConstraint`
2. Enforce witness/trace completeness:
- Every non-trivial processed inst edge has both witness and trace entries.
3. Make failures explicit `PresolutionError` constructors (no silent drops except already-documented trivial-scheme dropping).

Exit criteria:
1. `prConstraint` is a translatable presolution artifact for elaboration.
2. Contract checks fail-fast with deterministic diagnostics.
3. All tests pass.

## Phase C - `Solved` Projection-First Cutover

1. Migrate all consumers to original/projection query path.
- Replace canonical-node-map reads with `lookupNode` + `canonical` composition where needed.
- Primary targets:
  - [Reify/Core.hs](/Volumes/src/mlf4/src/MLF/Reify/Core.hs)
  - [Fallback.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs)
  - [Generalize.hs](/Volumes/src/mlf4/src/MLF/Elab/Generalize.hs)
  - [Elab/Run/Generalize.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize.hs)
2. Remove canonical-domain query exports from [Solved.hs](/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs) once call sites are migrated.
3. Keep `validateOriginalCanonicalAgreement` temporarily for cutover validation only; do not use as runtime behavior dependency.

Exit criteria:
1. No elaboration/reify consumer depends on canonical-domain node maps as primary source.
2. `Solved` API is projection-first by construction.
3. All tests pass.

## Phase D - Phi Thesis-Exact Witness-First Semantics

1. Make witness-domain-first strict, not advisory.
- In [IdentityBridge.hs](/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs):
  - Keep current ranking, but split APIs into explicit:
    - strict witness-domain candidates
    - class-fallback candidates
  - prevent implicit class fallback unless caller opts in.
2. In [Translate.hs](/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs):
- Use strict witness-domain resolution first for binder/source replay mapping.
- Use class fallback only in explicitly marked recovery branch.
3. In [Omega.hs](/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs):
- Keep current direct->trace->class order and make class fallback telemetry mandatory (counter/log hook in trace config).
- Treat class fallback resolution failure as translatability error, not silent acceptance.

Exit criteria:
1. Phi decisions are witness-first and deterministic.
2. Class fallback is rare, explicit, measurable.
3. All tests pass.

## Phase E - Elaboration Boundary Cleanup + Legacy Removal

1. Remove remaining mutation-like `Solved` rebuilds from active elaboration flow where feasible.
- Keep only pipeline setup-time projection rebuilding.
- For fallback/generalize local rebuilds, either eliminate or encapsulate behind pure projection function with no backend mutation semantics.
2. Make projection-first default and remove transitional dual-path entrypoint.
- Remove `runPipelineElabProjectionFirst` from [Pipeline.hs](/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs).
- Keep a test-only harness comparing historical baseline snapshots (not runtime API split).
3. Remove obsolete legacy helpers/exports in [Solved.hs](/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs) and dependent modules.
4. Update docs:
- [implementation_notes.md](/Volumes/src/mlf4/implementation_notes.md)
- [docs/thesis-deviations.yaml](/Volumes/src/mlf4/docs/thesis-deviations.yaml)
- [CHANGELOG.md](/Volumes/src/mlf4/CHANGELOG.md)

Exit criteria:
1. Single production path, thesis-exact semantics.
2. No legacy canonical-heavy branch API remains.
3. Documentation reflects final architecture.

## Test Cases and Scenarios

## Core invariant suite

1. Ordering invariant:
- Initial global closure before first inst edge.
- For each inst edge: propagation/edge execution then immediate closure.
- No pending unify edges at edge boundaries.
2. Artifact invariant:
- `prConstraint` has no `TyExp`, no `cUnifyEdges`, no `cInstEdges`.
3. Phi invariant:
- For each non-trivial edge, witness+trace exist and replay succeeds from witness domain first.

## Regression corpus

1. `id`, `const`, `app-id`, `let-poly`, `ann-id`, `nested-let`, higher-rank application.
2. Targeted weaken/raise/raise-merge cases for non-root binder replay.
3. Cases that previously needed class-member fallback to ensure explicit recovery path remains correct.

## Acceptance checks

1. `cabal build all && cabal test` passes.
2. No forbidden `Solved` canonical-domain query usage remains in elaboration/reify paths.
3. No runtime call sites to removed legacy APIs.
4. Output terms/types stable against frozen golden set for regression corpus.

## Rollout and Verification Gates

1. Gate A: sequencing refactor complete, no behavioral drift.
2. Gate B: presolution artifact contract enforced.
3. Gate C: projection-first consumer migration complete.
4. Gate D: Phi strict witness-first semantics complete.
5. Gate E: legacy APIs removed, docs finalized.

Each gate requires:
1. clean build/tests
2. targeted invariant suite pass
3. changelog/doc update for architectural change

## Assumptions and Defaults (Locked)

1. Scope: full A-E alignment.
2. Compatibility mode: aggressive cleanup.
3. Rollout: incremental phased implementation.
4. Source of truth for semantics: thesis (`papers/these-finale-english.txt`) over supplementary paper when conflict exists.
5. No repo mutations are part of this response; this is a planning artifact only.
