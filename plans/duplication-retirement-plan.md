# Plan — Duplication Retirement Sweep

Goal: remove duplicated logic in the internal implementation without changing semantics, by extracting shared utilities/modules and updating call sites incrementally (tests green after each step).

## Scope

In:
- Internal refactors under `src/MLF/**` (private `mlf2-internal` sublibrary).
- Test refactors under `test/**` to remove duplicated scaffolding.
- Small doc updates if names/modules move.

Out:
- Algorithmic changes (constraint generation / normalize / presolution / solve / elaboration behavior).
- Public API changes (keep `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`, `src-public/MyLib.hs` stable).

## Guardrails

- After each numbered item: run `cabal --config-file=.cabal-config test --test-show-details=direct`.
- Prefer “extract + rewrite imports” over “rewrite algorithms”.
- Avoid new dependency cycles; keep extracted helpers low-level and pure when possible.

## Worklist (attack one cluster at a time)

### 0) Baseline / audit
[x] Record current baseline: full test run, then `rg` for known duplication markers.
[x] For each cluster below, list exact call sites to migrate (so we don’t miss one).
    - 1) Test scaffolding: `test/GraphOpsSpec.hs`, `test/SolveSpec.hs`, `test/PresolutionSpec.hs`, `test/BindingSpec.hs`, `test/AcyclicitySpec.hs`, `test/NormalizeSpec.hs`
    - 2) structuralChildren: `src/MLF/Binding/Tree.hs`, `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Solve.hs`, `src/MLF/Constraint/Presolution/Core.hs` (now `.../Driver.hs`)
    - 3) occurs-check: `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Presolution/Unify.hs`, `src/MLF/Constraint/Solve.hs`
    - 4) canonicalize: `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Solve.hs`, `src/MLF/Constraint/Presolution/Driver.hs`
    - 5) reorder logic: `src/MLF/Elab/Sigma.hs`, `src/MLF/Elab/Phi.hs`
    - 6) library vs test: `test/ElaborationSpec.hs` (schemeToType usage)
    - 8) reachability: `src/MLF/Binding/Tree.hs`, `src/MLF/Constraint/Presolution/ForallIntro.hs`, `src/MLF/Frontend/ConstraintGen/Scope.hs`, `src/MLF/Constraint/Acyclicity.hs`
    - 9) max node id: `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Presolution/Driver.hs`
    - 10) order keys: `src/MLF/Binding/Tree.hs`, `src/MLF/Util/Order.hs`
    - 11) term-dag roots under UF: `src/MLF/Constraint/Presolution/Copy.hs`

### 1) Test scaffolding duplication (easy / high value)
Problem: `emptyConstraint`, `expectRight`/`requireRight`, node lookup helpers, and UF chase helpers are duplicated across spec files.

[x] Add `test/SpecUtil.hs` exporting:
  - `emptyConstraint :: Constraint`
  - `expectRight`, `requireRight`
  - small node lookup helpers (`lookupNodeIO` / `lookupNodeMaybe`)
[x] Replace local `emptyConstraint` definitions in:
  - `test/GraphOpsSpec.hs`, `test/SolveSpec.hs`, `test/PresolutionSpec.hs`, `test/BindingSpec.hs`, `test/AcyclicitySpec.hs`, `test/NormalizeSpec.hs`
[x] Replace test-local UF chase (e.g. `frWithUF`) with `MLF.Util.UnionFind.frWith` (or remove if no longer needed).
[x] Keep specs readable: only extract helpers that are used in ≥2 modules.

### 2) Structural traversal duplication (“children of TyNode”) (easy / foundational)
Problem: multiple hand-rolled “structural children” functions and ad-hoc case splits.

[x] Add a single canonical helper in `src/MLF/Constraint/Types.hs`:
  - `structuralChildren :: TyNode -> [NodeId]` (arrow/forall/exp children)
[x] Replace duplicates/ad-hoc logic in:
  - `src/MLF/Binding/Tree.hs` (there are multiple copies today)
  - `src/MLF/Constraint/Normalize.hs`
  - `src/MLF/Constraint/Solve.hs`
  - `src/MLF/Constraint/Presolution/Core.hs` (the “termDagRootsUnder” block)
[x] Ensure traversal order is preserved where it matters (arrow dom before cod, forall body, exp body).

### 3) Occurs-check / reachability duplication (medium)
Problem: 3 different occurs-check implementations across Normalize / Presolution / Solve.

[x] Introduce `src/MLF/Constraint/Traversal.hs` (or `MLF.Constraint.Graph`) exporting pure helpers:
  - `occursInUnder :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> NodeId -> Either TraversalError Bool`
    (read as: “does needle occur in target, following canonical reps?”)
  - optionally: `reachableFromUnder` / `foldReachableUnder` if useful
[x] Rewrite:
  - `src/MLF/Constraint/Normalize.hs` occurs check to call the shared helper
  - `src/MLF/Constraint/Presolution/Core.hs` occurs check to call the shared helper
  - `src/MLF/Constraint/Solve.hs` occurs check to call the shared helper
[x] Keep phase-specific error constructors intact (Normalize uses inst-edge handling; Solve uses `OccursCheckFailed`; Presolution uses `OccursCheckPresolution`).

### 4) “Canonicalize the constraint under UF” duplication (hard / staged)
Problem: Normalize, Presolution, and Solve all perform “rewrite to canonical reps” passes, but with different extra responsibilities (grafting, redirects/traces).

Approach: do not try to unify everything at once; extract common *pure rewriting* and leave phase-specific extras in place.

[x] Add `src/MLF/Constraint/Canonicalize.hs` exporting:
  - `rewriteInstEdges :: (NodeId -> NodeId) -> [InstEdge] -> [InstEdge]`
  - `rewriteUnifyEdges :: (NodeId -> NodeId) -> [UnifyEdge] -> [UnifyEdge]`
  - `rewriteBindParentsLenient :: (NodeId -> NodeId) -> (NodeId -> Bool) -> BindParents -> BindParents`
    (keeps first parent + max flag; preserves existing Solve/Normalize behavior)
  - `chooseRepNode :: TyNode -> TyNode -> TyNode` (prefer structure vs var)
[x] Migrate the “mechanical rewrite” parts of:
  - `src/MLF/Constraint/Solve.hs` `applyUFConstraint`
  - `src/MLF/Constraint/Normalize.hs` UF-application pass (but keep grafting where it is)
  - `src/MLF/Constraint/Presolution/Core.hs` `rewriteConstraint` (only for the “apply mapping to constraint” mechanics; keep trace logic local)
[x] Add regression assertions where possible:
  - after canonicalization, no edges refer to missing nodes
  - `checkBindingTree` still holds (or errors are preserved)
  Justification: Presolution rewrites now validate via `Binding.checkBindingTree`; Solve checks binding tree before/after and `validateSolvedGraph` covers missing nodes; Normalize stays pure and avoids introducing new failure paths.

### 5) Elaboration reorder logic duplication (medium)
Problem: two separate “bubble reorder” implementations:
- `sigmaReorderTo` (strings-only binder identities)
- `reorderBindersByPrec` (reorders `[Maybe NodeId]` + applies swaps)

[x] Extract a shared “bubble-to-desired-order” helper (kept in `src/MLF/Elab/Pipeline.hs` to share `swapAt` + `applyInstantiation`):
  - parameterized by:
    - how to find the desired element in the suffix
    - how to swap at depth k (`swapAt` + `applyInstantiation`)
[x] Rewrite `sigmaReorderTo` and `reorderBindersByPrec` to call it.
[x] Keep existing tests as regression (Σ reorder cases in `test/ElaborationSpec.hs` cover this path).

### 6) Library-vs-test duplication (small)
Problem: small helpers like `schemeToType` exist both in library and tests.

[x] Delete the test copy and reuse the library one:
  - export `schemeToType` from `MLF.Elab.Pipeline`
[x] Repeat for any other tiny duplicates found during the sweep.
  Justification: no other cross-test/library duplicates surfaced in the sweep beyond `schemeToType`.

### 7) Unification engine duplication (optional / last)
Problem: Normalize, Presolution edge-unify, and Solve have “unify-like” engines with overlapping mechanics but different invariants and side effects.

This is high risk; only do if duplication is truly painful.

[x] First, extract only pure “one-step structural decomposition” helpers (e.g. “Arrow vs Arrow produces child constraints”) into a shared module.
[x] Adopt those helpers in Normalize/Solve (leave Presolution’s edge witness logic alone initially).
[x] Re-run all tests; stop if any behavior changes appear.

### 8) Structural reachability duplication (easy)
Problem: multiple copies of “collect all nodes reachable from a root via structural children” exist.

Call sites today:
- `src/MLF/Binding/Tree.hs` (`structuralReachableFrom` used by `orderedBinders`)
- `src/MLF/Constraint/Presolution/Core.hs` (`structuralReachableFrom` used by `bindForallBindersFromSpec`)
- `src/MLF/Frontend/ConstraintGen.hs` (`structuralReachableFrom` used by `rebindScopeNodes`)
- `src/MLF/Constraint/Acyclicity.hs` (`collectReachableNodes` + local `getChildren`)

[x] Add `reachableFromUnderLenient` to `src/MLF/Constraint/Traversal.hs` (canonicalization + lookup; missing nodes treated as leafs).
[x] Migrate the four call sites above and delete their local implementations.
[x] Keep behavior identical (reachable set includes the root even if it is missing from `cNodes`).

### 9) Max NodeId scanning duplication (easy)
Problem: Normalize and Presolution both define `findMaxNodeId` helpers (plus other local max-id scans).

[x] Add `maxNodeIdKeyOr0 :: Constraint -> Int` to `src/MLF/Constraint/Types.hs` (max key in `cNodes`, default 0).
[x] Rewrite:
  - `src/MLF/Constraint/Normalize.hs` initial `nsNextNodeId` computation
  - `src/MLF/Constraint/Presolution/Core.hs` initial `psNextNodeId` computation
  - remove the duplicated local helpers.

### 10) ≺ order-key computation duplication (medium)
Problem: `src/MLF/Binding/Tree.hs` contains a full copy of the “leftmost-lowermost” order-key algorithm that also exists in `src/MLF/Util/Order.hs`.

Constraint: `MLF.Binding.Tree` must not import `MLF.Util.Order` because `MLF.Util.Order` imports Solve, and Solve imports Binding.Tree (cycle).

[x] Introduce `src/MLF/Util/OrderKey.hs` with the *pure* algorithm:
  - `OrderKey(..)`, `compareOrderKey`, `orderKeysFromRootWith`, `compareNodesByOrderKey`
  - keep the existing “allowed set” restriction behavior.
[x] Refactor `src/MLF/Util/Order.hs` to re-export the pure helpers from `MLF.Util.OrderKey` and keep only the SolveResult convenience wrappers.
[x] Refactor `src/MLF/Binding/Tree.hs` to use `MLF.Util.OrderKey` and delete its local `OrderKey`/`orderKeysFromRootWith`/`compareNodesByOrderKey` copy.

### 11) Term-DAG roots under canonicalization (small)
Problem: `bindUnboundCopiedNodes` computed term-DAG roots under UF by reimplementing the same structure-walk logic used by `Binding.Tree`.

[x] Add `computeTermDagRootsUnder :: (NodeId -> NodeId) -> Constraint -> IntSet` to `MLF.Binding.Tree`.
[x] Replace the local `termDagRootsUnder` computation in `MLF.Constraint.Presolution.Copy` with the shared helper.

### 12) Multi-root reachability (small)
Problem: `bindUnboundCopiedNodes` still computes reachability from a *set* of candidate nodes with a local BFS, duplicating logic from `Traversal.reachableFromUnderLenient`.

[x] Add `reachableFromManyUnderLenient` to `MLF.Constraint.Traversal` (multi-root union).
[x] Replace the local `reachableFrom` in `MLF.Constraint.Presolution.Copy` with the shared helper.

### 13) Canonicalized term-DAG roots inside Binding.Tree (small)
Problem: `checkBindingTreeUnder` recomputes canonicalized term-DAG roots inline instead of reusing `computeTermDagRootsUnder`.

[x] Replace the inline `termDagRoots` computation in `checkBindingTreeUnder` with `computeTermDagRootsUnder` (keeping `structEdges` for `isUpperUnder`).

### 14) Reify traversal duplication (small/medium)
Problem: `reifyType` and `reifyTypeWithNames` in `MLF.Elab.Reify` duplicate the same traversal/caching logic, differing only in variable naming and binder bound handling.

[x] Extract a shared `reifyTypeWith` helper that takes naming callbacks and binder collection.
[x] Keep existing behavior for bounds (only `reifyTypeWithNames` handles bounds).

### 15) Binding ancestor walk duplication (small/optional)
Problem: `bindingAncestors` inside `MLF.Constraint.Presolution.Driver.rewriteConstraint` reimplements a parent-chain walk similar to `Binding.bindingPathToRoot`.

[x] Replace the local walk with `bindingPathToRoot` (handle errors by falling back to `[]` to preserve current “best effort” behavior).

### 16) Normalize local NodeId accessor (tiny)
Problem: `getNodeIdInt` in `MLF.Constraint.Normalize` duplicates the `getNodeId` accessor.

[x] Drop `getNodeIdInt` and use `getNodeId` directly.

### 17) Elab binding-error wrapper duplication (tiny)
Problem: `firstBinding` in `MLF.Elab.Reify` and `MLF.Elab.Phi` duplicates the same `BindingTreeError` mapping (and similar inline mappings show up in `MLF.Elab.Generalize`).

[x] Extract a tiny helper (e.g. `bindingToElab :: Either BindingError a -> Either ElabError a`) into `src/MLF/Elab/Types.hs` or `src/MLF/Elab/Util.hs`.
[x] Replace `firstBinding` in:
  - `src/MLF/Elab/Reify.hs`
  - `src/MLF/Elab/Phi.hs`
  - (optional) the inline mapping in `src/MLF/Elab/Generalize.hs`

### 18) NodeId → IntMap lookup helper (tiny)
Problem: multiple modules define `lookupNode nid = IntMap.lookup (getNodeId nid) nodes`.

[x] Add a shared helper (e.g. `lookupNodeIn :: IntMap a -> NodeId -> Maybe a`) in a low-level module (`src/MLF/Constraint/Types.hs` or new `src/MLF/Constraint/Util.hs`).
[x] Replace the local helpers in:
  - `src/MLF/Constraint/Normalize.hs`
  - `src/MLF/Constraint/Presolution/Copy.hs`
  - `src/MLF/Constraint/Presolution/Unify.hs`

### 19) Binder ordering topo-sort duplication (medium)
Problem: three modules implement near-identical “order binders by dependency, break ties by ≺” topological sorts.

[x] Extract a shared helper for “stable topo-sort with order-key tie-breaks” (likely in `src/MLF/Elab/Util.hs` or `src/MLF/Elab/Pipeline.hs`).
[x] Replace the local implementations in:
  - `src/MLF/Elab/Reify.hs` (`orderUsedBinders`)
  - `src/MLF/Elab/Generalize.hs` (`orderBinderCandidates`)
  - `src/MLF/Elab/Phi.hs` (`desiredBinderOrder`)

## Validation

- Always: `cabal --config-file=.cabal-config test --test-show-details=direct`
- Also useful:
  - `rg -n "emptyConstraint ::|structuralChildren|occursIn" src test` to verify duplicates were removed.
  - `rg -n "structuralReachableFrom|findMaxNodeId" src/MLF` to ensure legacy local helpers are gone.
  - `rg -n --glob '!src/MLF/Util/OrderKey.hs' "data OrderKey = OrderKey" src/MLF` to ensure the ≺ key algorithm lives in one place.
  - `rg -n "reachableFromUnderLenient|maxNodeIdKeyOr0|OrderKey\\.orderKeysFromRootWith|Unify\\.Decompose" src/MLF` to confirm the shared helpers are the ones in use.
  - `cabal --config-file=.cabal-config build` for quick compile checks between steps.

## Risks

- Accidentally changing traversal order (affects ≺ ordering and some Φ placement).
- Introducing module cycles by extracting helpers into the wrong layer.
- Unifying “canonicalization” too aggressively (Normalize’s grafting is intentionally not just “rewrite IDs”).
