# Findings: Eliminate Solved Indirection

## Key Discovery: Solved is a thin UF+Constraint wrapper

`Solved` stores:
- The **original constraint** (= `prConstraint` from presolution, unchanged)
- A **canonical map** (= `prUnionFind` sanitized + collapsed)
- **Reconstructed canonical fields** (nodes, edges, bind-parents, gen-nodes) rebuilt by replaying the solve result from a snapshot

The replay (`solveResultFromSnapshot`) is the most complex part and only exists because `Solved.fromPreRewriteState` was designed for the legacy solve path, not the presolution path. For the presolution path, the constraint is already in its final form.

## Data flow from PresolutionResult to Solved

```
prConstraint ───→ ebOriginalConstraint  (stored as-is)
prUnionFind  ───→ ebCanonicalMap        (sanitized + chase function)
                  └─→ solveResultFromSnapshot → ebCanonical* fields
```

Only these 2 fields flow into Solved. The other 5 fields of PresolutionResult are threaded independently.

## Solved.canonical implementation

`canonical` chases the `ebCanonicalMap` IntMap with cycle-safe `equivCanonical`. This same function can operate directly on the `prUnionFind` map (after sanitization).

## Module impact heat map

| Cluster | Modules | Total Solved.* calls | Difficulty |
|---|---|---|---|
| Phi/Omega | 6 modules | ~37 | HIGH — Omega has ~30 calls across 5 APIs |
| Reify | 2 modules | ~26 | HIGH — Core.hs is dense with lookupVarBound |
| Elaborate | 1 module | ~12 | MEDIUM |
| Pipeline/Run | 6 modules | ~27 | MEDIUM — includes mutation patterns |
| Presolution/Plan | 4 modules | ~8 | MEDIUM — plan-builder captures Solved |
| Support | 3 modules | ~7 | LOW |

## Critical pattern: Plan-builder captures Solved

`prPlanBuilder` (in `PresolutionResult`) is a `PresolutionPlanBuilder` that takes a `Solved` argument when invoked. The call site is in `Elab/Run/Pipeline.hs` where `generalizeAtWith` passes the solved handle. This closure must be updated to accept `PresolutionView` instead.

## Unused Solved exports (can be dropped immediately)

`fromSolveOutput`, `fromPresolutionResult`, `allNodes`, `instEdges`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, `canonicalGenNodes`, `validateOriginalCanonicalAgreement`, `rebuildWithBindParents`, `rebuildWithGenNodes` — 12 functions with 0 call sites in `src/`.
