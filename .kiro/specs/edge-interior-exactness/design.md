# Design Document

## Overview
We align the implementation with `papers/xmlf.txt` (§3.2) by making `EdgeTrace.etInterior` the **exact** interior `I(r)` of the expansion root `r`:

> `I(r) = { n | n is transitively bound to r in the binding tree }`

The key change is to eliminate the current “superset” behavior (`exact ∪ approx`) and replace it with an interior computed directly from binding edges in **binding-edge mode**. Because presolution maintains a union-find over node equivalences before rewriting the term-DAG, the interior computation must be **quotient-aware** (operate on canonical representatives), matching `Binding.checkBindingTreeUnder`.

## Architecture
Interior computation is used in two places:

1) **Presolution edge-local processing**
   - to decide which raised nodes should be recorded as `OpRaise` (only those inside `I(r)`),
   - to restrict edge-local ≺ order keys to `I(r)` (paper’s “leftmost-lowermost” ordering inside the expansion).

2) **Elaboration Φ**
   - to restrict order key computation to `I(r)` when translating `OpRaise` via Figure 10 contexts.

```mermaid
flowchart LR
  P[Presolution edge processing] -->|compute I(r)| I[I(r) exact]
  I -->|filter OpRaise| W[EdgeWitness Ω]
  I -->|restrict ≺ keys| K[OrderKeys]
  W --> E[Elaboration Φ]
  I --> E
```

## Components and Interfaces

### 1) Quotient-aware interior (`MLF.Binding`)
Add a new API that mirrors `checkBindingTreeUnder`’s quotient semantics:

```hs
-- | Interior on the quotient graph induced by `canonical`.
-- The returned set contains canonical node ids.
interiorOfUnder
  :: (NodeId -> NodeId)
  -> Constraint
  -> NodeId  -- ^ root r (will be canonicalized internally)
  -> Either BindingError IntSet
```

Implementation approach:
- Build the **quotient binding-parent relation**:
  - rewrite each edge `child -> parent` to `(canonical child) -> (canonical parent)`
  - drop self-edges (`child == parent`)
  - merge duplicates (same child) by requiring same parent and taking `max flag` (same rule as `checkBindingTreeUnder`)
- Invert to **children adjacency** and perform a DFS/BFS from `canonical r` to collect descendants.
- Include `r` itself in the resulting set.

This yields an `O(|V| + |E|)` interior computation and avoids the current `Binding.interiorOf` implementation’s “check every node by walking its path” behavior.

### 2) Presolution: compute and use exact interior (`MLF.Presolution`)
Introduce an internal helper:

```hs
edgeInteriorExact
  :: NodeId  -- ^ root r
  -> PresolutionM IntSet  -- ^ canonical ids when binding-edge mode, otherwise approx
```

Rules:
- If `cBindParents` is non-empty:
  - use `canonical = UnionFind.frWith psUnionFind`
  - return `Binding.interiorOfUnder canonical psConstraint r`
- If `cBindParents` is empty:
  - keep existing approximate behavior derived from the expansion trace (legacy support)

Call sites to update:
- `processInstEdge`: use `edgeInteriorExact` as the `interior` argument to `initEdgeUnifyState` (so `OpRaise` recording and ≺ keys are driven by exact `I(r)`).
- `buildEdgeTrace`: record `etInterior = edgeInteriorExact r` (binding-edge mode); stop unioning with `args`/`binderMetas`.

### 3) Elaboration: rely on exact `etInterior` (existing)
No API changes required. `MLF.Elab.phiFromEdgeWitnessWithTrace` already uses `etInterior` as the restriction set when it is non-empty. After this change, that restriction becomes paper-faithful.

## Data Models
No new public data types are required.

Adjust documentation:
- `EdgeTrace.etInterior` should be documented as “nodes in `I(r)` (exact, binding-edge mode)” rather than “(approx.)”.

Optional (explicitly not required for this spec):
- Add a separate `etEdgeDomain` field if future work needs “extra nodes to consider” (e.g., arguments) without polluting `I(r)`.

## Error Handling
- `Binding.interiorOfUnder` returns `BindingError` (consistent with the rest of `MLF.Binding`).
- Presolution converts these into `PresolutionError` via the existing `BindingTreeError` constructor.
- In legacy mode (`cBindParents` empty), `edgeInteriorExact` shall not throw binding-tree errors.

## Testing Strategy
Prefer QuickCheck properties plus a small integration regression:

1) **Unit + property tests for `interiorOfUnder`**
   - Regression: small hand-built binding trees where the quotient collapses nodes.
   - Property: on generated small binding trees + canonicalization maps, `n ∈ interiorOfUnder canonical c r` iff `canonical r` appears on the binding-parent chain of `canonical n` in the quotient relation.

2) **Presolution regression: `EdgeTrace.etInterior` is exact**
   - Build a minimal presolution scenario containing an instantiation edge.
   - After `computePresolution`, assert `etInterior == Binding.interiorOf (prConstraint) (etRoot)` for that edge.

3) **Elaboration regression remains valid**
   - Re-run the existing “non-spine Raise” regression to ensure α-equivalence still holds with the stricter restriction set.

