# Implementation Plan

> Note: `cabal test` currently includes **intentional failing baselines** in `test/ElaborationSpec.hs`.
> For per-task verification, prefer `cabal test --test-show-details=direct --test-options='--match <pattern>'`.
> If `cabal test` cannot write its default build logs on your machine, prefix commands with `cabal --config-file=.cabal-config`.

- [x] 1. Add quotient-aware interior API (`Binding.interiorOfUnder`)
  - [x] 1.1 Add `interiorOfUnder` signature + implementation
    - Files: `src/MLF/Binding.hs`
    - Steps:
      - Add `interiorOfUnder :: (NodeId -> NodeId) -> Constraint -> NodeId -> Either BindingError IntSet`.
      - Implement by (a) building the quotient binding-parent map (same conflict rules as `checkBindingTreeUnder`), (b) inverting to child adjacency, and (c) BFS/DFS from `canonical r`.
      - Keep behavior deterministic (stable traversal order not required, but result set must be canonical ids).
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match interiorOfUnder'`
    - _Requirements: 1.1, 4.1_
  - [x] 1.2 Add unit regressions for quotient collapse cases
    - Files: `test/BindingSpec.hs`
    - Steps:
      - Add a regression where two nodes are canonicalized to the same rep and ensure `interiorOfUnder` includes only canonical ids.
      - Add a regression where a binding edge becomes a self-edge under canonicalization and is ignored.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"Binding.interiorOfUnder\"'`
    - _Requirements: 4.1_
  - [x] 1.3 Add QuickCheck property tests for `interiorOfUnder`
    - Files: `test/BindingSpec.hs`
    - Steps:
      - Reuse or extend existing binding-tree generators to produce small valid binding trees.
      - Generate a small canonicalization function (e.g., a partition map) and compute the quotient binding-parent relation in the test (reference implementation).
      - Property: for all canonical nodes `n`, `n ∈ interiorOfUnder canonical c r` iff `r` is on the binding-parent chain of `n` in the quotient relation.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"interiorOfUnder property\"'`
    - _Requirements: 4.1_

- [x] 2. Use exact `I(r)` during edge-local unification (no injected nodes)
  - [x] 2.1 Add `edgeInteriorExact` helper in presolution
    - Files: `src/MLF/Presolution.hs`
    - Steps:
      - Add `edgeInteriorExact :: NodeId -> PresolutionM IntSet`.
      - In binding-edge mode, compute `canonical = UnionFind.frWith psUnionFind` and call `Binding.interiorOfUnder canonical psConstraint r`.
      - In legacy mode, return the existing approximate interior derived from `(CopyMap, InteriorSet)` (keep behavior unchanged).
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"EdgeTrace\"'`
    - _Requirements: 1.2, 2.1_
  - [x] 2.2 Replace the current `interior = … ∪ args ∪ binderMetas` initialization
    - Files: `src/MLF/Presolution.hs`
    - Steps:
      - In `processInstEdge`, after `bindExpansionRootLikeTarget` + `bindUnboundCopiedNodes`, compute `interior <- edgeInteriorExact resNodeId`.
      - Pass that `interior` to `initEdgeUnifyState` (so `eusInteriorByRoot` tracks exactly `I(r)`).
      - Ensure any logic that still needs binder↦argument information uses `bas`/`binderMetas` rather than relying on `interior`.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match OpRaise'`
    - _Requirements: 2.1, 2.2_
  - [x] 2.3 Regression: `OpRaise` is not recorded for outside nodes
    - Files: `test/PresolutionSpec.hs`
    - Steps:
      - Add a small scenario where unification would raise both sides under symmetric harmonization, but only the interior side is recorded (filtering by `I(r)`).
      - Assert the recorded witness contains `OpRaise` only for nodes in `EdgeTrace.etInterior` (and not for the argument node).
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"OpRaise outside\"'`
    - _Requirements: 2.2_

- [x] 3. Record exact `I(r)` in `EdgeTrace` (remove union/superset)
  - [x] 3.1 Update `EdgeTrace` documentation and `buildEdgeTrace`
    - Files: `src/MLF/Presolution.hs`
    - Steps:
      - Update the `EdgeTrace` field comment to remove “(approx.)” in binding-edge mode.
      - In `buildEdgeTrace`, set `etInterior` to the exact interior computed from binding edges (prefer `edgeInteriorExact`), and remove the `IntSet.union interior approxInterior` logic.
      - Keep legacy fallback behavior when `cBindParents` is empty.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"EdgeTrace\"'`
    - _Requirements: 1.1, 1.2, 1.3_
  - [x] 3.2 Regression: `etInterior == Binding.interiorOf(etRoot)` on the final constraint
    - Files: `test/PresolutionSpec.hs`
    - Steps:
      - After `computePresolution`, pick an edge trace and compute `Binding.interiorOf (prConstraint) (etRoot)`.
      - Assert it equals `etInterior` exactly (same set).
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match \"EdgeTrace exact\"'`
    - _Requirements: 1.1, 4.2_

- [x] 4. Ensure Φ regressions still pass with exact interior restriction
  - [x] 4.1 Re-run (and update if needed) the existing non-spine Raise elaboration regression
    - Files: `test/ElaborationSpec.hs` (only if updates are needed)
    - Steps:
      - Ensure the “non-spine Raise” case still passes with the stricter `I(r)` restriction.
      - If the test relied on injected interior nodes, update the fixture to make those nodes truly inside `I(r)` (via binding edges) instead.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match non-spine'`
    - _Requirements: 3.2_
