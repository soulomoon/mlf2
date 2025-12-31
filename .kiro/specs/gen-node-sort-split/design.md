# Design Document

## Overview
Refactor the constraint data model to separate gen nodes (G) from type nodes, aligning
with `papers/these-finale-english.txt` where gen nodes are a distinct sort and the
constraint is rooted at a gen node. This removes the need for synthetic roots and
prepares the pipeline for thesis-exact scoping and context computation.

## Architecture
- Introduce a distinct `GenNodeId` and `GenNode` map in the constraint.
- Represent binding edges over a unified `NodeRef = Gen | Type` so gen nodes can
  bind only gen nodes, and type nodes are bound on gen nodes.
- Enforce rootedness: a single root gen node exists from Phase 1 onward.
- Treat `TyForall` purely as a type constructor; gen nodes introduce schemes via
  the binding tree rather than by reusing `TyForall` nodes as gen nodes.

## Data Model
- New identifiers:
  - `GenNodeId`, `TypeNodeId` (or keep `NodeId` as type node id).
  - `NodeRef = GenRef GenNodeId | TypeRef NodeId` for binding edges.
- New structures:
  - `GenNode { gnId, gnSchemes :: [NodeId] }` where schemes are type roots.
  - `cGenNodes :: IntMap GenNode`.
  - `cBindParents :: BindParents` updated to use `NodeRef`.

## Invariants (Thesis)
- A single root gen node exists and remains the only binding root for gen nodes.
- Gen nodes are only bound on gen nodes.
- All type nodes are bound (directly or transitively) on some gen node.
- Gen nodes are not merged/raised/instantiated; they are structural anchors only.

## Migration Strategy
1. Add explicit gen-node structures in the constraint (transitional compatibility).
2. Introduce `NodeRef` binding edges and migrate binding-tree operations.
3. Remove synthetic root insertion; enforce rootedness at constraint generation.
4. Migrate elaboration and Phi/Sigma translation to use gen nodes as the source of
   named nodes and contexts.
5. Remove legacy reliance on `TyForall`/`TyRoot` as gen nodes.

## Error Handling
- If a constraint lacks a root gen node, fail fast with a structured error.
- If a gen node is bound under a type node, reject the constraint as ill-formed.
- If a type node is not bound under any gen node, reject the constraint.

## Testing Strategy
- Unit tests for gen-node invariants (rootedness, binding constraints).
- Regression tests for named-node enumeration using gen nodes only.
- End-to-end tests to ensure elaboration and Phi still match thesis examples.

## References
- `papers/these-finale-english.txt` (gen nodes, rooted constraints, contexts)
- `papers/xmlf.txt` (binding tree, Phi translation)
